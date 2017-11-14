-module(ws_client).

-behaviour(websocket_client_handler).

-export([
         send/2,
         send_async/2,
         recv/1,
         start_link/1,
         stop/1
        ]).

-export([
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-define(TIMEOUT, 1000).

%%% api

start_link(Url) ->
  Ref = make_ref(),
  State = [{url, Url},
           {ref, Ref},
           {caller, self()}],
  {ok, Pid} = websocket_client:start_link(Url, ?MODULE, State),
  {ok, #{pid => Pid, ref => Ref}}.

stop(#{pid := ConnPid}) ->
  ConnPid ! stop.

send(#{pid := ConnPid} = Conn, Msg) ->
  MsgJson = jsx:encode(Msg),
  websocket_client:cast(ConnPid, {text, MsgJson}),
  recv(Conn).

send_async(#{pid := ConnPid}, Msg) ->
  MsgJson = jsx:encode(Msg),
  websocket_client:cast(ConnPid, {text, MsgJson}).

recv(#{ref := Ref}) ->
    receive
      {ws_client, reply, Ref, AnswerMsg} ->
        {ok, AnswerMsg}
    after ?TIMEOUT ->
        {error, timeoutp}
    end.

%%% websocket client callbacks

init(State, _ConnState) ->
  {ok, State}.

websocket_handle({text, MsgJson}, _ConnState, State) ->
  Caller = proplists:get_value(caller, State),
  Ref = proplists:get_value(ref, State),

  Msg = jsx:decode(MsgJson, [return_maps]),
  Caller ! {ws_client, reply, Ref, Msg},
  {ok, State}.

websocket_info(stop, _, State) ->
  {close, <<>>, State};
websocket_info(_Msg, _ConnState, State) ->
  {ok, State}.

websocket_terminate(_Msg, _ConnState, _State) ->
  ok.
