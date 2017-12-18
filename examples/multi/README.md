# WebRTC multi-party example

Cowboy application that serves an HTML client for WebRTC.
An unlimited number of peers can join a conversation by entering the
same room URL.
A mesh topology is used, meaning that if there are N peers, each one
will maintain N - 1 RTC peer connections. This is CPU intensive and
will work with a low number of peers.

## Run in development

    make dev

The example app will run on `https://localhost:8443/:room`

## Run in production

To run the example app stand alone in a production server, update the
relevant configuration in `conf/sys.config` (port, certs, host, etc.)
and run:

    make release

Unpack the generated tar and run `bin/webrtc_server start`.
