'use strict';

var isChannelReady = false;
var isInitiator = false;
var isStarted = false;
var localStream;
var pc;
var remoteStream;
var turnReady;


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

// Set up audio and video regardless of what devices are present.
var sdpConstraints = {
  offerToReceiveAudio: true,
  offerToReceiveVideo: true
};

/////////////////////////////////////////////

const room = window.location.pathname.split(/\//g)[1] ;
console.log("room is", room);
const wsUrl = 'ws://' + window.location.host + '/websocket/' + room;
const socket = new WebSocket(wsUrl);

function sendMessage(event, message) {
  const payload = {
    event,
    data: message
  };
  console.log('Client sending message: ', event, message);
  socket.send(JSON.stringify(payload));
}

// fake event listeners ala socket.io
const listeners = {
  created,
  joined,
  candidate,
  offer,
  answer,
  gotMedia,
  bye
};

socket.onopen = function(event) {
  console.log('socket connected');

  socket.onmessage = function(e) {
    const data = JSON.parse(e.data);
    console.log('Client received message:', data);
    listeners[data.event](data.data);
  };
};

function created () {
  console.log('Created room ');
  isInitiator = true;
}

function joined () {
  console.log('joined: ' + room);
  isChannelReady = true;
}

function candidate(data) {
  if (isStarted) {
    var candidate = new RTCIceCandidate({
      sdpMLineIndex: data.label,
      candidate: data.candidate
    });
    pc.addIceCandidate(candidate);
  }
}

function offer (data) {
  if (!isInitiator && !isStarted) {
    maybeStart();
  }
  pc.setRemoteDescription(new RTCSessionDescription(data));
  doAnswer();
}

function answer (data) {
  if (isStarted) {
    pc.setRemoteDescription(new RTCSessionDescription(data));
  }
}

function bye () {
  if (isStarted) {
    handleRemoteHangup();
  }
}


function gotMedia () {
  maybeStart();
}

////////////////////////////////////////////////////

var localVideo = document.querySelector('#localVideo');
var remoteVideo = document.querySelector('#remoteVideo');

navigator.mediaDevices.getUserMedia({
  audio: true,
  video: true
})
.then(gotStream)
.catch(function(e) {
  console.log(e.stack);
  alert('getUserMedia() error: ' + e.name);
});

function gotStream(stream) {
  console.log('Adding local stream.');
  localVideo.srcObject = stream;
  localStream = stream;
  sendMessage('gotMedia');
  if (isInitiator) {
    maybeStart();  }
}

var constraints = {
  video: true
};

console.log('Getting user media with constraints', constraints);

function maybeStart() {
  console.log('>>>>>>> maybeStart() ', isStarted, localStream, isChannelReady);
  if (!isStarted && typeof localStream !== 'undefined' && isChannelReady) {
    console.log('>>>>>> creating peer connection');
    createPeerConnection();
    pc.addStream(localStream);
    isStarted = true;
    console.log('isInitiator', isInitiator);
    if (isInitiator) {
      doCall();
    }
  }
}

window.onbeforeunload = function() {
  sendMessage('bye');
};

/////////////////////////////////////////////////////////

function createPeerConnection() {
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
}

function handleIceCandidate(event) {
  console.log('icecandidate event: ', event);
  if (event.candidate) {
    if(event.candidate.candidate.indexOf("relay")<0){
      // console.log("SKIPPING NON TURN CANDIDATE")
      // return;
    }
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

function doCall() {
  console.log('Sending offer to peer');
  pc.createOffer(setLocalAndSendMessage, handleCreateOfferError);
}

function doAnswer() {
  console.log('Sending answer to peer.');
  pc.createAnswer().then(
    setLocalAndSendMessage,
    onCreateSessionDescriptionError
  );
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

function hangup() {
  console.log('Hanging up.');
  stop();
  sendMessage('bye');
}

function handleRemoteHangup() {
    console.log('Session terminated.');
    stop();
    isInitiator = false;
}

function stop() {
  isStarted = false;
  // isAudioMuted = false;
  // isVideoMuted = false;
  pc.close();
  pc = null;
}
