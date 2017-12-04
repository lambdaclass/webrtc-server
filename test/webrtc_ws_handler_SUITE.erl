-module(webrtc_ws_handler_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% callbacks
auth_fun(_Username) ->
  %% password hardcoded for all users
  <<"password">>.

create_callback(Room, Username, OtherUsers) ->
  ets:insert(callback_log, {Username, create, Room,  OtherUsers}).

join_callback(Room, Username, OtherUsers) ->
  ets:insert(callback_log, {Username, join, Room,  OtherUsers}).

leave_callback(Room, Username, OtherUsers) ->
  ets:insert(callback_log, {Username, leave, Room,  OtherUsers}).

all() ->
  [
   join_and_send_message,
   auth_failure,
   callbacks,
   ping
  ].

init_per_suite(Config) ->
  Port = 8444,
  start_cowboy(8444),
  application:set_env(webrtc_server, auth_fun, {webrtc_ws_handler_SUITE, auth_fun}),
  application:ensure_all_started(webrtc_server),
  [{port, Port} | Config].

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  User1 = random_name(<<"User1">>),
  User2 = random_name(<<"User2">>),
  Room = random_name(<<"Room">>),
  Port = proplists:get_value(port, Config),
  PortBin = integer_to_binary(Port),
  Url = <<"wss://localhost:", PortBin/binary, "/websocket/", Room/binary>>,

  [{user1, User1},
   {user2, User2},
   {room, Room},
   {url, Url} | Config].

end_per_testcase(_TestCase, _Config) ->
  ok.

join_and_send_message(Config) ->
  Url = proplists:get_value(url, Config),
  User1 = proplists:get_value(user1, Config),
  User2 = proplists:get_value(user2, Config),

  %% user 1 joins and auths -> created
  {ok, Conn1} = ws_client:start_link(Url),
  AuthData = #{<<"event">> => <<"authenticate">>,
               <<"data">> => #{<<"username">> => User1,
                               <<"password">> => <<"password">>}},
  {ok, #{<<"event">> := <<"created">>}} = ws_client:send(Conn1, AuthData),

  %% user 2 joins and auths -> joined
  {ok, Conn2} = ws_client:start_link(Url),
  AuthData2 = #{<<"event">> => <<"authenticate">>,
               <<"data">> => #{<<"username">> => User2,
                               <<"password">> => <<"password">>}},
  {ok, #{<<"event">> := <<"joined">>}} = ws_client:send(Conn2, AuthData2),
  {ok, #{<<"event">> := <<"joined">>}} = ws_client:recv(Conn1),

  %% user 1 sends message, user 2 receives
  ws_client:send(Conn1, #{<<"event">> => <<"hello!">>}),
  {ok, #{<<"event">> := <<"hello!">>}} = ws_client:recv(Conn2),

  %% user 2 sends message, user 1 receives
  ws_client:send(Conn2, #{<<"event">> => <<"bye!">>}),
  {ok, #{<<"event">> := <<"bye!">>}} = ws_client:recv(Conn1),

  %% user 2 leaves -> left
  ws_client:stop(Conn2),
  {ok, #{<<"event">> := <<"left">>}} = ws_client:recv(Conn1),

  ok.

auth_failure(Config) ->
  Url = proplists:get_value(url, Config),
  User1 = proplists:get_value(user1, Config),
  User2 = proplists:get_value(user2, Config),

  %% user 1 joins and auths -> created
  {ok, Conn1} = ws_client:start_link(Url),
  AuthData = #{<<"event">> => <<"authenticate">>,
               <<"data">> => #{<<"username">> => User1,
                               <<"password">> => <<"WRONG!">>}},
  {ok, #{<<"event">> := <<"unauthorized">>}} = ws_client:send(Conn1, AuthData),

  %% user 2 joins and auths -> joined
  {ok, Conn2} = ws_client:start_link(Url),
  AuthData2 = #{<<"event">> => <<"authenticate">>,
                <<"data">> => #{<<"username">> => User2,
                                <<"password">> => <<"password">>}},
  {ok, #{<<"event">> := <<"created">>}} = ws_client:send(Conn2, AuthData2),
  ok.

callbacks(Config) ->
  application:set_env(webrtc_server, create_callback, {webrtc_ws_handler_SUITE, create_callback}),
  application:set_env(webrtc_server, join_callback, {webrtc_ws_handler_SUITE, join_callback}),
  application:set_env(webrtc_server, leave_callback, {webrtc_ws_handler_SUITE, leave_callback}),

  Url = proplists:get_value(url, Config),
  User1 = proplists:get_value(user1, Config),
  User2 = proplists:get_value(user2, Config),
  Room = proplists:get_value(room, Config),

  ets:new(callback_log, [named_table, public, bag]),

  %% user 1 creates the room
  {ok, Conn1} = ws_client:start_link(Url),
  AuthData = #{<<"event">> => <<"authenticate">>,
               <<"data">> => #{<<"username">> => User1,
                               <<"password">> => <<"password">>}},
  {ok, #{<<"event">> := <<"created">>}} = ws_client:send(Conn1, AuthData),

  %% user 2 joins
  {ok, Conn2} = ws_client:start_link(Url),
  AuthData2 = #{<<"event">> => <<"authenticate">>,
                <<"data">> => #{<<"username">> => User2,
                                <<"password">> => <<"password">>}},
  {ok, #{<<"event">> := <<"joined">>}} = ws_client:send(Conn2, AuthData2),
  {ok, #{<<"event">> := <<"joined">>}} = ws_client:recv(Conn1),

  %% users leave the room
  ws_client:stop(Conn1),
  timer:sleep(100),
  ws_client:stop(Conn2),
  timer:sleep(100),

  [{User1, create, Room, []},
   {User1, leave, Room, [User2]}] = ets:lookup(callback_log, User1),
  [{User2, join, Room, [User1]},
   {User2, leave, Room, []}] = ets:lookup(callback_log, User2),
  ok.

ping(Config) ->
  Url = proplists:get_value(url, Config),
  User1 = proplists:get_value(user1, Config),
  {ok, Conn1} = ws_client:start_link(Url),

  %% ping before auth
  {ok, <<"pong">>} = ws_client:ping(Conn1),

  %% ping after auth
  AuthData = #{<<"event">> => <<"authenticate">>,
               <<"data">> => #{<<"username">> => User1,
                               <<"password">> => <<"password">>}},
  {ok, #{<<"event">> := <<"created">>}} = ws_client:send(Conn1, AuthData),
  {ok, <<"pong">>} = ws_client:ping(Conn1),
  ok.

%% internal
start_cowboy(Port) ->
  application:ensure_all_started(cowboy),
  {ok, Cert} = application:get_env(webrtc_server, certfile),
  {ok, Key} = application:get_env(webrtc_server, keyfile),
  Dispatch = cowboy_router:compile([{'_', [{"/websocket/:room", webrtc_ws_handler, []}]}]),
  {ok, _} = cowboy:start_tls(my_http_listener,
                             [{port, Port},
                              {certfile, Cert},
                              {keyfile, Key}],
                             #{env => #{dispatch => Dispatch}}).

random_name(Prefix) ->
  Random = rand:uniform(10000),
  Sufix = integer_to_binary(Random),
  <<Prefix/binary, Sufix/binary>>.
