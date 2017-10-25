-module(example_auth).
-export([authenticate/1]).

authenticate(_Username) ->
  {ok, Password} = application:get_env(webrtc_server, example_password),
  Password.
