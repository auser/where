-module (where).

-behaviour(application).
-define (APPLICATIONS_TO_START, [chordjerl]).
%% application callbacks
-export([start/2, stop/1]).
-export ([init/1, layers_receive/1]).
-export ([lookup/1]).

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

lookup(Key) ->
  gen_server:call(where_server, {lookup, Key}).
  
layers_receive(Msg) ->
  case Msg of
    {whisper, Socket, Unencrypted} ->
      case Unencrypted of
        {lookup, Key} ->
          Val = lookup(Key),
          Reply = converse:reply(Socket, {reply, Val}),
          Reply;
        Else ->
          io:format("where layers_receive: ~p~n", [Else])
      end;
    Anything ->
      io:format("layers_receive Received: ~p (in ~p)~n", [Anything, ?MODULE])
  end.

start(Type, Config) ->    
    layers:start_bundle([
      {"Applications", fun() -> [A:start(Type, Config) || A <- ?APPLICATIONS_TO_START] end},
      {"Where server", fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]) end}
    ]).

init([Config]) ->
  TcpServerSup = { where_server, {where_server,start_link,[Config]}, permanent,2000,worker,[]},
  {ok, {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME}, [TcpServerSup]}}.

stop(State) -> ok.
