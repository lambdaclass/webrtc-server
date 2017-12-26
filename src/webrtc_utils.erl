-module(webrtc_utils).

-export([json_encode/1,
         json_decode/1,
         text_event/1,
         text_event/2]).

json_decode(Data) ->
  jsx:decode(Data, [return_maps, {labels, attempt_atom}]).

json_encode(Data) ->
  jsx:encode(Data).

text_event(Event) ->
  {text, json_encode(#{event => Event})}.

text_event(Event, Data) ->
  {text, json_encode(#{event => Event, data => Data})}.
