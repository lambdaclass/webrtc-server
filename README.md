# webrtc_erlang

An OTP application to make video calls using [WebRTC](https://webrtc.org/). It includes a STUN/TURN server using [processone/stun](https://github.com/processone/stun).

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

### Firewall setup for STUN/TURN

``` 
iptables -A INPUT -p tcp --dport 3478 -j ACCEPT
iptables -A INPUT -p udp --dport 3478 -j ACCEPT
iptables -A INPUT -p tcp --dport 5349 -j ACCEPT
iptables -A INPUT -p udp --dport 5349 -j ACCEPT
iptables -A INPUT -p udp --dport 49152:65535 -j ACCEPT
``` 

### Build a release

    make release
    
Unpack the generated tar and `bin/webrtc_erlang start`.
