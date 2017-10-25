'use strict';

var isInitiator = false;
var isStarted = false;
var localStream;
var pc;
var remoteStream;

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
    data: message
  };
  console.log('Client sending message: ', event, message);
  socket.send(JSON.stringify(payload));
}

//// SOCKET EVENT LISTENERS

// when theres one client connected, server consider the rooms as 'created'
// thus, if a client gets created, it's the initiator
function created () {
  console.log('Created room, this client is initiator');
  isInitiator = true;
}

// we're asssuming 1on1 conversations. when a client gets 'joined', then both
// clients are connected => channel ready
function joined () {
  console.log('joined: ' + room);

  // both users connected, so now we can move to next step, intiate the RTC communication
  startRTC();
}

function candidate(data) {
  var candidate = new RTCIceCandidate({
    sdpMLineIndex: data.label,
    candidate: data.candidate
  });
  pc.addIceCandidate(candidate);
}

function offer (data) {
  pc.setRemoteDescription(new RTCSessionDescription(data));
  console.log('Sending answer to peer.');
  pc.createAnswer().then(
    function(sessionDescription) {
      pc.setLocalDescription(sessionDescription);
      sendMessage('offer', sessionDescription);
    },
    logEvent('Failed to create session description:')
  );
}

function answer (data) {
  pc.setRemoteDescription(new RTCSessionDescription(data));
}

function bye () {
  if (isStarted) {
    console.log('Session terminated.');
    isStarted = false;
    pc.close();
    pc = null;

    // assumption: if other client leaves and this one stays, it becomes the initiator
    // if another users attempts to join again
    isInitiator = true;
  }
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
    created,
    joined,
    candidate,
    offer,
    answer,
    bye
  };

  socket.onmessage = function(e) {
    const data = JSON.parse(e.data);
    console.log('Client received message:', data);
    const listener = listeners[data.event];
    if (listener) {
      listener(data.data);
    } else {
      console.log('no listener for message', data.event);
    }
  };

  window.onbeforeunload = function() {
    sendMessage('bye');
  };
}

////////////////////////////////////////////////////

function startRTC() {
  console.log('>>>>>> creating peer connection');

  try {
    pc = new RTCPeerConnection(pcConfig);
    pc.onicecandidate = handleIceCandidate;
    pc.ontrack = handleRemoteStreamAdded;
    pc.onremovestream = logEvent('Remote stream removed,');
    console.log('Created RTCPeerConnnection');

    pc.addStream(localStream);
    isStarted = true;

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
  remoteStream = event.streams[0];
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
