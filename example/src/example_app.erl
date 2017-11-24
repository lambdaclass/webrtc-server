-module(example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/js/[...]", cowboy_static, {priv_dir, example, "/js"}},
                                           {"/css/[...]", cowboy_static, {priv_dir, example, "/css"}},
                                           {"/websocket/:room", webrtc_ws_handler, []},
                                           {'_', cowboy_static, {priv_file, example, "/index.html"}}
                                          ]}
                                   ]),
  {ok, _} = cowboy:start_tls(my_http_listener,
                             [{port, config(port)},
                              {certfile, config(certfile)},
                              {keyfile, config(keyfile)}
                             ],
                             #{env => #{dispatch => Dispatch}}
                            ),
  example_sup:start_link().

stop(_State) ->
    ok.

%% Internal functions
config(Key) ->
  {ok, Value} = application:get_env(example, Key),
  Value.
