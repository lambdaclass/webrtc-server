-module(webrtc_server).

-export([peers/1,
         publish/3,
         send/3]).

peers(Room) ->
  [{PeerId, Name} || {_Pid, {Name, PeerId}} <- syn:get_members(Room, with_meta)].

publish(Room, Event, Data) ->
  Message = webrtc_utils:text_event(Event, Data),
  syn:publish(Room, Message).

send(Peer, Event, Data) ->
  Pid = syn:find_by_key(Peer),
  Message = webrtc_utils:text_event(Event, Data),
  Pid ! Message,
  ok.
