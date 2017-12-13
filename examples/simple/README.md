# WebRTC simple example

Simple Cowboy application that serves an HTML client for WebRTC.
Connections are established by two peers that enter in the same room URL.
The client connects to the signaling and ICE servers provided by webrtc_server.

## Run in development

    make dev

The example app will run on `https://localhost:8443/:room`

## Run in production

To run the example app stand alone in a production server, update the
relevant configuration in `conf/sys.config` (port, certs, host, etc.)
and run:

    make release

Unpack the generated tar and run `bin/webrtc_server start`.
