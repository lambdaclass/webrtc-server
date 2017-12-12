'use strict';

var localStream;
var pc;
var remotePeerId;

var username = 'username';
var password = 'password';

var localVideo = document.querySelector('#localVideo');
var remoteVideo = document.querySelector('#remoteVideo');

const stunUrl = 'stun:' + window.location.hostname + ':3478';
const turnUrl = 'turn:' + window.location.hostname + ':3478';
var pcConfig = {
  iceServers: [{
    urls: turnUrl,
    username: username,
    credential: password
  },{
    urls: stunUrl
  }]
};

/**
 * General algorithm:
 * 1. get local stream
 * 2. connect to signaling server
 * 3. authenticate and wait for other client to connect
 * 4. when both peers are connected (socker receives joined message),
 *    start webRTC peer connection
 */
getStream()
  .then(() => connectSocket())
  .catch(console.log);

function getStream() {
  return navigator.mediaDevices
    .getUserMedia({
      audio: true,
      video: true
    })
    .then(function (stream) {
      console.log('Adding local stream.');
      localVideo.srcObject = stream;
      localStream = stream;
    })
    .catch(function(e) {
      console.log(e.stack);
      alert('getUserMedia() error: ' + e.name);
    });
}

/////////////////////////////////////////////

const room = window.location.pathname.split(/\//g)[1] ;
console.log("room is", room);
const wsUrl = 'wss://' + window.location.host + '/websocket/' + room;
var socket;

// helper to send ws messages with {event, data} structure
function sendMessage(event, message) {
  const payload = {
    event,
    data: message,
    to: remotePeerId
  };
  console.log('Client sending message: ', payload);
  socket.send(JSON.stringify(payload));
}

//// SOCKET EVENT LISTENERS

function authenticated (data) {
  console.log('authenticated:', data.peer_id);
}

// we're asssuming 1on1 conversations. when a second client joins then both
// clients are connected => channel ready
function joined(data) {
  console.log('peer joined', data.peer_id);
  remotePeerId = data.peer_id;

  // this is the initiator
  startRTC(true);
}

function candidate(data) {
  var candidate = new RTCIceCandidate({
    sdpMLineIndex: data.label,
    candidate: data.candidate
  });
  pc.addIceCandidate(candidate);
}

function offer (data, fromPeer) {
  // received offer from the other peer, start as receiver
  remotePeerId = fromPeer;
  startRTC(false);

  pc.setRemoteDescription(new RTCSessionDescription(data));
  console.log('Sending answer to peer.');
  pc.createAnswer().then(
    function(sessionDescription) {
      pc.setLocalDescription(sessionDescription);
      sendMessage('answer', sessionDescription);
    },
    logEvent('Failed to create session description:')
  );
}

function answer (data) {
  pc.setRemoteDescription(new RTCSessionDescription(data));
}

function left () {
  console.log('Session terminated.');
  if (pc) {
    pc.close();
    pc = null;
    remotePeerId = undefined;
  }
  remoteVideo.srcObject = undefined;
}

/*
 * Connect the socket and set up its listeners.
 * Will return a promise that resolves once both clients are connected.
 */
function connectSocket() {
  // setting global var, sorry
  socket = new WebSocket(wsUrl);

  socket.onopen = function(event) {
    console.log('socket connected');
    sendMessage('authenticate', {username, password});
  };

  socket.onclose = function(event) {
    console.log('socket was closed', event);
  };

  const listeners = {
    authenticated,
    joined,
    left,
    candidate,
    offer,
    answer
  };

  socket.onmessage = function(e) {
    const data = JSON.parse(e.data);
    console.log('Client received message:', data);
    const listener = listeners[data.event];
    if (listener) {
      listener(data.data, data.from);
    } else {
      console.log('no listener for message', data.event);
    }
  };

}

////////////////////////////////////////////////////

function startRTC(isInitiator) {
  console.log('>>>>>> creating peer connection');

  try {
    pc = new RTCPeerConnection(pcConfig);
    pc.onicecandidate = handleIceCandidate;
    pc.ontrack = handleRemoteStreamAdded;
    pc.onremovestream = logEvent('Remote stream removed,');
    console.log('Created RTCPeerConnnection');

    pc.addStream(localStream);

    if (isInitiator) {
      createOffer();
    }
  } catch (e) {
    console.log('Failed to create PeerConnection, exception: ' + e.message);
    alert('Cannot create RTCPeerConnection object.');
    return;
  }
}

//// PeerConnection handlers

function handleIceCandidate(event) {
  console.log('icecandidate event: ', event);
  if (event.candidate) {
    sendMessage('candidate', {
      label: event.candidate.sdpMLineIndex,
      id: event.candidate.sdpMid,
      candidate: event.candidate.candidate
    });
  } else {
    console.log('End of candidates.');
  }
}

function handleRemoteStreamAdded(event) {
  console.log('Remote stream added.');
  remoteVideo.srcObject = event.streams[0];
}

function createOffer() {
  console.log('Sending offer to peer');
  pc.createOffer(function(sessionDescription) {
    pc.setLocalDescription(sessionDescription);
    sendMessage('offer', sessionDescription);
  }, logEvent('createOffer() error:'));
}

// event/error logger
function logEvent(text) {
  return function (data) {
    console.log(text, data);
  };
}
