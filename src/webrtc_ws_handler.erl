-module(webrtc_ws_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

init(Req, _) ->
  Room = cowboy_req:binding(room, Req),
  IdleTimeout = application:get_env(webrtc_server, idle_timeout, 60000),
  {cowboy_websocket, Req,
   #{room => Room, authenticated => false},
   #{idle_timeout => IdleTimeout}}.

websocket_init(State) ->
  Time = application:get_env(webrtc_server, ws_auth_delay, 300),

  %% give the ws some time to authenticate before disconnecting it
  timer:send_after(Time, check_auth),
  {ok, State}.

%% not all ws clients can send a ping frame (namely, browsers can't)
%% so we handle a ping text frame.
websocket_handle({text, <<"ping">>}, State) ->
  {reply, {text, <<"pong">>}, State};

%% Before authentication, just expect the socket to send user/pass
websocket_handle({text, Text}, State = #{authenticated := false, room := Room}) ->
   case authenticate(Text) of
     {success, Username} ->
       lager:debug("socket authenticated"),

       PeerId = peer_id(),
       join_room(Room, Username, PeerId),

       State2 = State#{authenticated => true,
                       username => Username,
                       peer_id => PeerId},

       {reply, reply_text(authenticated, #{peer_id => PeerId}), State2};
     Reason ->
       lager:debug("bad authentication: ~p ~p", [Reason, Text]),
       {reply, reply_text(unauthorized), State}
   end;

%% After authentication, any message should be targeted to a specific peer
websocket_handle({text, Text}, State = #{authenticated := true,
                                         room := Room,
                                         peer_id := ThisPeer}) ->
  lager:debug("Received text frame ~p", [Text]),

  case json_decode(Text) of
    #{to := OtherPeer} = Message ->
      %% crash if room doesn't match
      {Pid, {_Username, _PeerId, Room}} = syn:find_by_key(OtherPeer, with_meta),

      %% extend message with this peer id before sending
      Message2 = Message#{from => ThisPeer},
      Pid ! {text, json_encode(Message2)},

      {ok, State};
    _ ->
      {reply, reply_text(invalid_message), State}
  end;

websocket_handle(Frame, State) ->
  lager:warning("Received non text frame ~p~p", [Frame, State]),
  {ok, State}.

%% If user/password not sent before ws_auth_delay, disconnect
websocket_info(check_auth, State = #{authenticated := false}) ->
  lager:debug("disconnecting unauthenticated socket"),
  {stop, State};

websocket_info(check_auth, State) ->
  %% already authenticated, do nothing
  {ok, State};

%% incoming test frame, send to the client socket
websocket_info({text, Text}, State = #{authenticated := true}) ->
  lager:debug("Sending to client ~p", [Text]),
  {reply, {text, Text}, State};

websocket_info(Info, State) ->
  lager:warning("Received unexpected info ~p~p", [Info, State]),
  {ok, State}.

terminate(_Reason, _Req, #{room := Room, username := Username, peer_id := PeerId}) ->
  OtherUsers = [Name || {Pid, {Name, _PeerId}} <- syn:get_members(Room, with_meta), Pid /= self()],
  run_callback(leave_callback, Room, Username, OtherUsers),
  syn:publish(Room, reply_text(left, #{username => Username, peer_id => PeerId})),
  ok;
terminate(_Reason, _Req, _State) ->
  ok.

%%% internal
authenticate(Data) ->
  try json_decode(Data) of
    #{event := <<"authenticate">>, data := #{username := User, password := Password}} ->
      case safe_auth(User) of
        Password -> {success, User};
        _ -> wrong_credentials
      end;
    _ -> invalid_format
  catch
    Type:Error ->
      lager:debug("invalid json ~p ~p", [Type, Error]),
      invalid_json
  end.

safe_auth(Username) ->
  {ok, {AuthMod, AuthFun}} = application:get_env(webrtc_server, auth_fun),
  try
    AuthMod:AuthFun(Username)
  catch
    _:_ ->
      auth_error
  end.

join_room(Room, Username, PeerId) ->
  OtherMembers = syn:get_members(Room, with_meta),
  syn:register(PeerId, self(), {Username, PeerId, Room}),
  syn:join(Room, self(), {Username, PeerId}),

  OtherNames = [Name || {_, {Name, _Peer}} <- OtherMembers],
  run_callback(join_callback, Room, Username, OtherNames),

  %% broadcast peer joined to the rest of the peers in the room
  Message = reply_text(joined, #{peer_id => PeerId, username => Username}),
  lists:foreach(fun({Pid, _}) -> Pid ! Message end, OtherMembers).

run_callback(Type, Room, Username, CurrentUsers) ->
  case application:get_env(webrtc_server, Type) of
    {ok, {Module, Function}} ->
      try
        Module:Function(Room, Username, CurrentUsers)
      catch
        ErrorType:Error ->
          lager:warning("Error running ~p callback ~p/~p: ~p ~p",
                        [Type, Room, Username, ErrorType, Error])
      end;
    undefined ->
      ok
  end.

reply_text(Event) ->
  {text, json_encode(#{event => Event})}.

reply_text(Event, Data) ->
  {text, json_encode(#{event => Event, data => Data})}.

peer_id() ->
  base64:encode(crypto:strong_rand_bytes(10)).

json_decode(Data) ->
  jsx:decode(Data, [return_maps, {labels, attempt_atom}]).

json_encode(Data) ->
  jsx:encode(Data).
