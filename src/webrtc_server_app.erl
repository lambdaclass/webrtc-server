-module(webrtc_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API

start(_StartType, _StartArgs) ->
  syn:init(),
  stun_listener:add_listener(3478, udp, [{use_turn, true},
                                         {turn_ip, resolve_ip()},
                                         {certfile, config(certfile)},
                                         {auth_type, user},
                                         {auth_realm, config(hostname)},
                                         {auth_fun, get_stun_auth_fun()}]),
  webrtc_server_sup:start_link().

stop(_State) ->
  ok.

%% Internal functions
config(Key) ->
  {ok, Value} = application:get_env(webrtc_server, Key),
  Value.

resolve_ip() ->
  case application:get_env(webrtc_server, turn_ip) of
    {ok, IP} ->
      IP;
    undefined ->
      Host = case config(hostname) of
               H when is_binary(H) -> binary_to_list(H);
               H when is_list(H) -> H;
               _ ->
                 lager:error("hostname should be a string or binary"),
                 throw(bad_hostname)
             end,
      [IP | _] = inet_res:lookup(Host, in, a),
      IP
end.

get_stun_auth_fun() ->
  {AuthMod, AuthFun} = config(auth_fun),
  fun (User, _Realm) ->
      lager:debug("Stun authentication for  ~p", [User]),
      %% the stun app considers <<"">> as an auth failure
      try
        case AuthMod:AuthFun(User) of
          Password when is_binary(Password) -> Password;
          _ -> <<"">>
        end
      catch
        _:_ -> <<"">>
      end
  end.
