'use strict';

let localStream;
let peerId;
const peers = {};

var username = 'username';
var password = 'password';

var localVideo = document.querySelector('#localVideo');

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
function sendMessage(event, message, toPeer) {
  const payload = {
    event,
    data: message
  };
  if (toPeer) {
    payload.to = toPeer;
  }
  console.log('Client sending message: ', event, message);
  socket.send(JSON.stringify(payload));
}

//// SOCKET EVENT LISTENERS

function authenticated (data) {
  peerId = data.peer_id;
  console.log('authenticated:', peerId);
}

function joined (data) {
  console.log('peer joined', data.peer_id);
  // start RTC as initiator with newly joined peer
  startRTC(data.peer_id, true);
}

function offer (data, fromPeer) {
  // received an offer, need to initiate rtc as receiver before answering
  startRTC(fromPeer, false);

  const connection = peers[fromPeer].connection;
  connection.setRemoteDescription(new RTCSessionDescription(data));
  console.log('Sending answer to peer.');
  connection.createAnswer().then(
    function(sessionDescription) {
      connection.setLocalDescription(sessionDescription);
      sendMessage('answer', sessionDescription, fromPeer);
    },
    logEvent('Failed to create session description:')
  );
}

function candidate(data, fromPeer) {
  var candidate = new RTCIceCandidate({
    sdpMLineIndex: data.label,
    candidate: data.candidate
  });
  peers[fromPeer].connection.addIceCandidate(candidate);
}

function answer (data, fromPeer) {
  peers[fromPeer].connection.setRemoteDescription(new RTCSessionDescription(data));
}

function left (data) {
  console.log('Session terminated.');
  const otherPeer = data.peer_id;
  peers[otherPeer].connection.close();

  // remove dom element
  const element = document.getElementById(peers[otherPeer].element);
  element.srcObject = undefined;
  element.parentNode.removeChild(element);

  delete peer[otherPeer];
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

function startRTC(peerId, isInitiator) {
  console.log('>>>>>> creating peer connection');

  try {
    const connection = new RTCPeerConnection(pcConfig);

    connection.onicecandidate = getHandleIceCandidate(peerId);
    connection.ontrack = getHandleRemoteStream(peerId);
    connection.onremovestream = logEvent('Remote stream removed,');

    connection.addStream(localStream);

    peers[peerId] = {connection};

    console.log('Created RTCPeerConnnection for', peerId);

    if (isInitiator) {
      createOffer(peerId);
    }
  } catch (e) {
    console.log('Failed to create PeerConnection, exception: ' + e.message);
    alert('Cannot create RTCPeerConnection object.');
    return;
  }
}

//// PeerConnection handlers

function getHandleIceCandidate(peerId) {
  return function(event) {
    console.log('icecandidate event: ', event);
    if (event.candidate) {
      sendMessage('candidate', {
        label: event.candidate.sdpMLineIndex,
        id: event.candidate.sdpMid,
        candidate: event.candidate.candidate
      }, peerId);
    } else {
      console.log('End of candidates.');
    }
  };
}

function getHandleRemoteStream(peerId) {
  return function(event) {
    console.log('Remote stream added for peer', peerId);
    const elementId = "video-" + peerId;

    // this handler can be called multiple times per stream, only
    // add a new video element once
    if (!peers[peerId].element) {
      const t = document.querySelector('#video-template');
      t.content.querySelector('video').id = elementId;
      const clone = document.importNode(t.content, true);
      document.getElementById("videos").appendChild(clone);
      peers[peerId].element = elementId;
    }
    // always set the srcObject to the latest stream
    document.getElementById(elementId).srcObject = event.streams[0];
  };
}

function createOffer(peerId) {
  console.log('Sending offer to peer');
  const connection = peers[peerId].connection;
  connection.createOffer(function(sessionDescription) {
    connection.setLocalDescription(sessionDescription);
    sendMessage('offer', sessionDescription, peerId);
  }, logEvent('createOffer() error:'));
}

// event/error logger
function logEvent(text) {
  return function (data) {
    console.log(text, data);
  };
}
