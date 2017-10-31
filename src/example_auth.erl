-module(example_auth).
-export([authenticate/1]).

authenticate(Username) ->
  lager:info("Authenticating ~p", [Username]),
  case Username of
    <<"error">> -> error;
    <<"crash">> -> throw(auth_error);
    _ ->
      {ok, Password} = application:get_env(webrtc_server, example_password),
      Password
  end.
