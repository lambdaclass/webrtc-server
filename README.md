# webrtc-server [![Build Status](https://travis-ci.org/lambdaclass/webrtc-server.svg?branch=master)](https://travis-ci.org/lambdaclass/webrtc-server)

An OTP application to make video calls using [WebRTC](https://webrtc.org/). It includes a STUN/TURN server using [processone/stun](https://github.com/processone/stun).

## Configuration
### authentication

An authentication function needs to be provided to the app
environment as which will be used to authenticate both the websocket
connections to the signaling server and the turn connections.

Example:

``` erlang
{auth_fun, {module, function}}
```

The function will ba called like `module:function(Username)`, and
should return the expected password for the given Username. The
password will compared to the one sent by the clients. Auth will be
considered failed if the result value is not a binary or the function
throws an error.

The implementation of this function will depend on how this
application is expected to be deployed. It could just return a fixed
password from configuration, encode the username using a secret shared
with the application server, lookup the password on a common
datastore, etc.

### callbacks
webrtc_server allows to define callback functions that will be
triggered when users enter or leave a room. This can be useful to
track conversation state (such as when a call starts or end), without
needing extra work in the clients.

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

* port: port used to serve the signaling server and the example client.
* certfile: path to the certificate file for the signaling server and
  example client.
* keyfile: path to the key file for the signaling server and
  example client.
* hostname: name of the host where the app will be deployed. Will be
  used as the `auth_realm` for stun and to lookup the `turn_ip`
* turn_ip: IP of the server where the app will be deployed. If not
  provided, will default to the first result of
  `inet_res:lookup(Hostname, in, a)`
* idle_timeout: cowboy configuration for the websocket
  connections. By default will disconnect idle sockets after a
  minute (thus requiring the clients to periodically send a ping). Use
  `infinity` to prevent disconnections.

## Websockets API for signaling

TODO

## Run in development

    make dev

The example app will run on `https://localhost:8443/:room`

## Run in production

To run the example app stand alone in a production server, update the
relevant configuration in `conf/sys.config` (port, certs, host, etc.)
and run:

    make release

Unpack the generated tar and `bin/webrtc_server start`.

Alternatively, the webrtc_server application can be included as a
dependency in another project.

### Firewall setup for STUN/TURN

```
iptables -A INPUT -p tcp --dport 3478 -j ACCEPT
iptables -A INPUT -p udp --dport 3478 -j ACCEPT
iptables -A INPUT -p tcp --dport 5349 -j ACCEPT
iptables -A INPUT -p udp --dport 5349 -j ACCEPT
iptables -A INPUT -p udp --dport 49152:65535 -j ACCEPT
```

## Mac build error

This error while building on mac OS:

```
_build/default/lib/fast_tls/c_src/fast_tls.c:21:10: fatal error: 'openssl/err.h' file not found
```

Is solved by exporting some openssl flags:

```
export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CFLAGS="-I/usr/local/opt/openssl/include/"
export CPPFLAGS="-I/usr/local/opt/openssl/include/"
```
