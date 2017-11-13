-module(webrtc_ws_handler_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [
   join_and_send_message,
   auth_failure,
   create_callback,
   join_callback,
   leave_callback
  ].

auth_fun(_Username) ->
  %% password hardcoded for all users
  <<"password">>.

init_per_suite(Config) ->
  application:set_env(webrtc_server, auth_fun, {webrtc_ws_handler_SUITE, auth_fun}),
  application:ensure_all_started(webrtc_server),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  User1 = random_name(<<"User1">>),
  User2 = random_name(<<"User2">>),
  Room = random_name(<<"Room">>),
  Url = <<"wss://localhost:8443/websocket/", Room/binary>>,
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

auth_failure(_Config) ->
  ok.

create_callback(_Config) ->
  ok.

join_callback(_Config) ->
  ok.

leave_callback(_Config) ->
  ok.

%% internal
random_name(Prefix) ->
  Random = rand:uniform(10000),
  Sufix = integer_to_binary(Random),
  <<Prefix/binary, Sufix/binary>>.
