%%%-------------------------------------------------------------------
%%% File    : where_server.erl
%%% Author  : Ari Lerner
%%% Description : where.erl.erl 
%%%
%%% Created :  Fri Feb 27 12:30:17 PST 2009
%%%-------------------------------------------------------------------

-module (where_server).
-behaviour(gen_server).

%% API
-export ([start/2]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (where_state, {
                       successor,    % layers successor
                       module        % chord | ...
                     }).

-define(SERVER, ?MODULE).
-define (DEFAULT_CONFIG, [
                            {module, chordjerl}
                          ]). 

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(_Type, Config) ->
  start_link(Config).
  
start_link(Config) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Config]) ->
  [Successor, Mod] = config:fetch_or_default_config([successor,module], Config, ?DEFAULT_CONFIG),
  {ok, #where_state{
              successor = Successor,
              module = Mod
              }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({lookup, Key}, From, #where_state{successor = Successor, module = Mod} = State) ->
  Val = Mod:lookup(Key),
  case Successor of
    undefined -> where:layers_receive({lookup, Val});
    Suc -> spawn_link(fun() -> layers:pass(Successor, {lookup, Val}) end)
  end,
  {reply, Val, State};
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
