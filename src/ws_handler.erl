-module(ws_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(Req, _) ->
  Room  = cowboy_req:binding(room, Req),
  {cowboy_websocket, Req, #{room => Room}}.

websocket_init(State) ->
  Room = maps:get(room, State),
  syn:join(Room, self()),

  %% client needs to know if it's the initiator
  Members = syn:get_members(Room),
  case length(Members) of
    1 ->
      {reply, reply_text(created), State};
    _ ->
      syn:publish(Room, reply_text(joined)),
      {ok, State}
  end.

websocket_handle(Frame = {text, Text}, State) ->
  lager:debug("Received text frame ~p", [Frame]),

  %% send to all other pids in group
  Room = maps:get(room, State),
  Members = syn:get_members(Room),
  Self = self(),
  Send = fun(Pid) when Pid /= Self -> Pid ! {text, Text};
            (_Pid) -> ok
         end,

  lists:foreach(Send, Members),
  {ok, State};
websocket_handle(Frame, State) ->
  lager:warn("Received non text frame ~p", [Frame]),
  {ok, State}.

websocket_info({text, Text}, State) ->
  lager:debug("Sending to client ~p", [Text]),
  {reply, {text, Text}, State};
websocket_info(Info, State) ->
  lager:warn("Received unexpected info ~p", [Info]),
  {ok, State}.

%%% internal
reply_text(Event) ->
  {text, jsx:encode(#{event => Event})}.
