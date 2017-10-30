-module(webrtc_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/js/[...]", cowboy_static, {priv_dir, webrtc_server, "/js"}},
                                           {"/css/[...]", cowboy_static, {priv_dir, webrtc_server, "/css"}},
                                           {"/websocket/:room", ws_handler, []},
                                           {'_', cowboy_static, {priv_file, webrtc_server, "/index.html"}}
                                          ]}
                                   ]),
  {ok, _} = cowboy:start_tls(my_http_listener,
                             [{port, config(port)},
                              {certfile, config(certfile)},
                              {keyfile, config(keyfile)}
                             ],
                             #{env => #{dispatch => Dispatch}}
                            ),
  syn:init(),
  stun_listener:add_listener(3478, udp, [{use_turn, true},
                                         {certfile, config(certfile)},
                                         {auth_type, user},
                                         {auth_realm, config(auth_realm)},
                                         {auth_fun, get_stun_auth_fun()}]),
  webrtc_server_sup:start_link().

stop(_State) ->
  ok.

%% Internal functions
config(Key) ->
  {ok, Value} = application:get_env(webrtc_server, Key),
  Value.

get_stun_auth_fun() ->
  {AuthMod, AuthFun} = config(auth_fun),
  fun (User, _Realm) ->
      AuthMod:AuthFun(User)
  end.
