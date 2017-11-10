-module(example).
-export([authenticate/1,
         create/3,
         join/3,
         leave/3]).

authenticate(_Username) ->
  %% in a real scenario this may lookup the password in the db, request an external service, etc.
  {ok, Password} = application:get_env(webrtc_server, example_password),
  Password.

create(Room, Username, _OtherUsers) ->
  lager:info("~s created ~s", [Username, Room]).

join(Room, Username, _OtherUsers) ->
  lager:info("~s joined ~s", [Username, Room]).

leave(Room, Username, _OtherUsers) ->
  lager:info("~s left ~s", [Username, Room]).
