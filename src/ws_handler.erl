-module(ws_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(Req, _) ->
  Room = cowboy_req:binding(room, Req),
  {cowboy_websocket, Req, #{room => Room, authenticated => false}}.

websocket_init(State) ->
  Time = application:get_env(webrtc_server, ws_auth_delay, 300),

  %% give the ws some time to authenticate before disconnecting it
  timer:send_after(Time, check_auth),
  {ok, State}.

websocket_handle({text, Text}, State = #{authenticated := false}) ->
   case authenticate(Text) of
     success ->
       lager:debug("socket authenticated"),
       State2 = State#{authenticated => true},
       Room = maps:get(room, State2),
       CreatedOrJoined = join_room(Room),
       syn:publish(Room, reply_text(CreatedOrJoined)),
       {ok, State2};
     Reason ->
       lager:debug("bad authentication: ~p ~p", [Reason, Text]),
       {reply, reply_text(unauthorized), State}
   end;
websocket_handle({text, Text}, State = #{authenticated := true}) ->
  lager:debug("Received text frame ~p", [Text]),

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
  lager:warning("Received non text frame ~p~p", [Frame, State]),
  {ok, State}.

websocket_info(check_auth, State = #{authenticated := false}) ->
  lager:debug("disconnecting unauthenticated socket"),
  {stop, State};
websocket_info(check_auth, State) ->
  %% already authenticated, do nothing
  {ok, State};
websocket_info({text, Text}, State = #{authenticated := true}) ->
  lager:debug("Sending to client ~p", [Text]),
  {reply, {text, Text}, State};
websocket_info(Info, State) ->
  lager:warning("Received unexpected info ~p~p", [Info, State]),
  {ok, State}.

%%% internal
reply_text(Event) ->
  {text, jsx:encode(#{event => Event})}.

authenticate(Data) ->
  try jsx:decode(Data, [return_maps, {labels, attempt_atom}]) of
    #{event := <<"authenticate">>, data := #{username := User, password := Password}} ->
      case safe_auth(User) of
        %% FIXME allowing auth error here just to test turn, REMOVE
        error -> success;
        auth_error -> success;
        Password -> success;
        _ -> wrong_credentials
      end;
    _ -> invalid_format
  catch
    Type:Error ->
      lager:debug("invalid json ~p ~p", [Type, Error]),
      invalid_json
  end.

join_room(Room) ->
  syn:join(Room, self()),
  Members = syn:get_members(Room),
  case length(Members) of
    1 -> created;
    _ -> joined
  end.

safe_auth(Username) ->
  {ok, {AuthMod, AuthFun}} = application:get_env(webrtc_server, auth_fun),
  try
    AuthMod:AuthFun(Username)
  catch
    _:_ ->
      auth_error
  end.
