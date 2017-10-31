# webrtc-server

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
password will compared to the one sent by the clients.

The implementation of this function will depend on how this
application is expected to be deployed. It could just return a fixed
password from configuration, encode the username using a secret shared
with the application server, lookup the password on a common
datastore, etc.

## Websockets API for signaling

TODO

## Run in development

    make dev

The app will run on `https://localhost:8443/:room`

## Run in production

### Configuration
Update the configuration in `conf/sys.config`:

* port: set to 443 for HTTPS.
* certfile: absolute path to the certificate file.
* keyfile: absolute path to the key file.
* auth_realm: domain where it will be deployed, required for turn.
* turn_ip: IP of the server where it will be deployed, required for turn.

### Build a release

    make release

Unpack the generated tar and `bin/webrtc_server start`.

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
