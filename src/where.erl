-module (where).

-behaviour(application).
-define (APPLICATIONS_TO_START, []).
%% application callbacks
-export([start/2, stop/1]).
-export ([init/1]).
-export ([lookup/1]).

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

lookup(Key) ->
  gen_server:call(where_server, {lookup, Key}).

start(_Type, Config) ->    
    layers:start_bundle([
      {"Applications", fun() -> [application:start(A) || A <- ?APPLICATIONS_TO_START] end},
      {"Where server", fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]) end}
    ]).

init([Config]) ->
  TcpServerSup = { where_server, {where_server,start_link,[Config]}, permanent,2000,worker,[]},
  {ok, {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME}, [TcpServerSup]}}.

stop(State) -> ok.
