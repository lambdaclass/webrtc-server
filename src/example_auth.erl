-module(example_auth).
-export([authenticate/1]).

authenticate(_Username) ->
  %% in a real scenario this may lookup the password in the db, request an external service, etc.
  {ok, Password} = application:get_env(webrtc_server, example_password),
  Password.
