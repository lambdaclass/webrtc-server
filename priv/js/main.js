'use strict';

var isInitiator = false;
var isStarted = false;
var localStream;
var pc;
var remoteStream;

var localVideo = document.querySelector('#localVideo');
var remoteVideo = document.querySelector('#remoteVideo');

const stunUrl = 'stun:' + window.location.hostname + ':3478';
const turnUrl = 'turn:' + window.location.hostname + ':3478';
var pcConfig = {
  'iceServers': [{
    'urls': turnUrl,
    'username': 'username',
    'credential': 'credential'
  },{
    urls: stunUrl
  }]
};

/**
 * General algorithm:
 * 1. get local stream
 * 2. connect to signaling server and wait for other client to connect
 * 3. when both peers are connected, start webRTC peer connection
 */
getStream()
  .then(connectSocket)
  .then(startRTC)
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

var socket;

function sendMessage(event, message) {
  const payload = {
    event,
    data: message
  };
  console.log('Client sending message: ', event, message);
  socket.send(JSON.stringify(payload));
}

/*
 * Connect the socket and set up its listeners.
 * Will return a promise that resolves once both clients are connected.
 */
function connectSocket() {
  const room = window.location.pathname.split(/\//g)[1] ;
  console.log("room is", room);
  const wsUrl = 'wss://' + window.location.host + '/websocket/' + room;

  // setting global var, sorry
  socket = new WebSocket(wsUrl);
  return new Promise(function (resolve, reject) {
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

      // both users connected, so now we can move to next step
      resolve();
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
        setLocalAndSendMessage,
        onCreateSessionDescriptionError
      );
    }

    function answer (data) {
      pc.setRemoteDescription(new RTCSessionDescription(data));
    }

    function bye () {
      if (isStarted) {
        console.log('Session terminated.');
        stop();
        isInitiator = false;
      }
    }

    const listeners = {
      created,
      joined,
      candidate,
      offer,
      answer,
      bye
    };

    socket.onopen = function(event) {
      console.log('socket connected');
    };

    socket.onmessage = function(e) {
      const data = JSON.parse(e.data);
      console.log('Client received message:', data);
      listeners[data.event](data.data);
    };
  });
}

////////////////////////////////////////////////////

function startRTC() {
  console.log('>>>>>> creating peer connection');
  try {
    pc = new RTCPeerConnection(pcConfig);
    pc.onicecandidate = handleIceCandidate;
    pc.ontrack = handleRemoteStreamAdded;
    pc.onremovestream = handleRemoteStreamRemoved;
    console.log('Created RTCPeerConnnection');
  } catch (e) {
    console.log('Failed to create PeerConnection, exception: ' + e.message);
    alert('Cannot create RTCPeerConnection object.');
    return;
  }
  pc.addStream(localStream);
  isStarted = true;

  if (isInitiator) {
    console.log('Sending offer to peer');
    pc.createOffer(setLocalAndSendMessage, handleCreateOfferError);
  }
}

window.onbeforeunload = function() {
  sendMessage('bye');
};

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

function handleCreateOfferError(event) {
  console.log('createOffer() error: ', event);
}

function setLocalAndSendMessage(sessionDescription) {
  pc.setLocalDescription(sessionDescription);
  console.log('setLocalAndSendMessage sending message', sessionDescription);

  //use sessionDescription.type (offer/answer) as the event
  sendMessage(sessionDescription.type, sessionDescription);
}

function onCreateSessionDescriptionError(error) {
  trace('Failed to create session description: ' + error.toString());
}

function handleRemoteStreamAdded(event) {
  console.log('Remote stream added.');
  if(!remoteVideo.srcObject) {
    remoteVideo.srcObject = event.streams[0];
    remoteStream = event.streams[0];
  }
}

function handleRemoteStreamRemoved(event) {
  console.log('Remote stream removed. Event: ', event);
}

function stop() {
  isStarted = false;
  pc.close();
  pc = null;
}
