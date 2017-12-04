# webrtc-server [![Build Status](https://travis-ci.org/lambdaclass/webrtc-server.svg?branch=master)](https://travis-ci.org/lambdaclass/webrtc-server)

An Erlang/OTP application that provides all the server side components to make video calls using [WebRTC](https://webrtc.org/).
## Usage

Add `webrtc_server` as a dependency, for example using rebar3:

``` erlang
{deps, [
        {cowboy, "2.0.0"},
        {webrtc_server, {git, "https://github.com/lambdaclass/webrtc-server", {ref, "56bce3"}}}
       ]}
```

`webrtc_server` provides a single Cowboy WebSocket handler
`webrtc_ws_handler` that acts as the Signaling Server to pass data
between peers. Add the handler to your cowboy 2.0 application:

``` erlang
Dispatch = cowboy_router:compile([{'_', [{"/websocket/:room", webrtc_ws_handler, []}]}]),
{ok, _} = cowboy:start_tls(my_http_listener,
                           [{port, config(port)},
                            {certfile, config(certfile)},
                            {keyfile, config(certkey)}],
                           #{env => #{dispatch => Dispatch}}).
```

Note the handler expects a `:room` parameter to group connecting
clients into different rooms.

In addition to the Signaling server, `webrtc_server` starts a
STUN/TURN server on port 3478
using [processone/stun](https://github.com/processone/stun), which can
be used as ICE servers by the WebRTC peers. A browser client can use it like:

``` javascript
var pc = new RTCPeerConnection({
  iceServers: [{
    urls: "stun:example.com:3478"
  },{
    urls: "turn:example.com:3478",
    username: "username",
    credential: "password"
  }]
});
```

The [example directory](https://github.com/lambdaclass/webrtc-server/tree/master/example)
contains a full Cowboy application using `webrtc_server` and a simple
browser client that establishes a WebRTC connection using the
Signaling and ICE servers.

## Configuration
### authentication

An authentication function needs to be provided to the app
environment to authenticate both the websocket
connections to the signaling server and the TURN connections.

Example:

``` erlang
{auth_fun, {module, function}}
```

This function will ba called like `module:function(Username)`, and
should return the expected password for the given Username. The
password will be compared to the one sent by the client. Authentication will be
considered failed if the result value is not a binary or the function
throws an error.

The implementation of the function will depend on how the webrtc
application is expected to be deployed. It could just return a fixed
password from configuration, encode the username using a secret shared
between the webtrtc and application servers, lookup the password on a common
datastore, etc.

### callbacks
webrtc_server allows to define callback functions that will be
triggered when users enter or leave a room. This can be useful to
track conversation state (such as when a call starts or ends), without
needing extra work from the clients.

``` erlang
{create_callback, {module, function}}
{join_callback, {module, function}}
{leave_callback, {module, function}}
```

The three callbacks receive the same arguments:

* Room: name of the room used to connect.
* Username: username provided by the client executing the action.
* OtherUsers: list of the rest of the usernames currently in the room.

### server configuration

* certfile: path to the certificate file for the STUN server.
* keyfile: path to the key file for for the STUN server.
* hostname: webrtc server hostname. Will be used as the `auth_realm`
  for TURN and to lookup the `turn_ip` if it's not provided.
* turn_ip: IP of the webrtc server. If not provided, will default to
  the first result of `inet_res:lookup(Hostname, in, a)`.
* idle_timeout: [Cowboy option](https://ninenines.eu/docs/en/cowboy/2.0/manual/cowboy_websocket/#_opts) for the websocket
  connections. By default will disconnect idle sockets after a
  minute (thus requiring the clients to periodically send a ping  message). Use
  `infinity` to disable idle timeouts.

## Signaling API reference
### Authentication
After connection, an authentication JSON message should be sent:

``` json
{
    "event": "authenticate",
    "data": {
        "username": "john",
        "password": "s3cr3t!"
    }
}
```

### Room messages

After authentication, any (non ping) message sent will be forwarded to
the rest of the clients connected to the room.

### Ping
To send a keepalive message to prevent idle connections to be droped
by the server, send a plain text frame of value `ping`. The server
will respond with `pong`.

## Troubleshooting
### openssl error during compilation

```
_build/default/lib/fast_tls/c_src/fast_tls.c:21:10: fatal error: 'openssl/err.h' file not found
```

On debian it's solved by installing libssl-dev:

```
sudo apt-get install libssl-dev
```

On macOS it's solved by exporting some openssl flags:

```
export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CFLAGS="-I/usr/local/opt/openssl/include/"
export CPPFLAGS="-I/usr/local/opt/openssl/include/"
```

### Firewall setup for STUN/TURN

```
iptables -A INPUT -p tcp --dport 3478 -j ACCEPT
iptables -A INPUT -p udp --dport 3478 -j ACCEPT
iptables -A INPUT -p tcp --dport 5349 -j ACCEPT
iptables -A INPUT -p udp --dport 5349 -j ACCEPT
iptables -A INPUT -p udp --dport 49152:65535 -j ACCEPT
```
