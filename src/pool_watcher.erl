%% Copyright LineMetrics 2013
-module(pool_watcher).
-author("Alexander Minichmair").

-behaviour(gen_server).

-include("../include/ezk_pool.hrl").

%% API
-export([start_link/0, set_pool_name/1]).

%% gen_server callbacks
-export([init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {poolname}).

%%%===================================================================
%%% API
%%%===================================================================


set_pool_name(NewName) ->
   gen_server:call(?MODULE, {poolname, NewName}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
   {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term()} | ignore).
init([]) ->
   %% maybe remonitor client pids
   case lets:read_list(monitored_pids, ?MODULE) of
      [] -> ok;
      [_P|_R] = Pids -> [new_watch(false, Pid) || Pid <- Pids]
   end,
   {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
   {reply, Reply :: term(), NewState :: #state{}} |
   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_call({poolname, NewName}, _From, State) ->
   {reply, ok, State#state{poolname = NewName}};
handle_call(_Request, _From, State) ->
   {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({new_watch, EzkWorker}, State) ->
   ?LOG("new watch for worker: ~p, existing monitors: ~p",[EzkWorker, erlang:process_info(self(), monitors)]),
   maybe_new_watch(EzkWorker),
   {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_info({'DOWN', _MonitorRef, process, WorkerPid, _Info}=R, #state{poolname = PoolName}=State) ->
   %% start a new ezk connection
   ?LOG("got a 'DOWN' message from monitored process(ezk_pool_worker) ~p", [R]),
   %% tell the next pool-worker process to takeover
   %% all the watch_paths from the process that has just gone down
%%    ok = poolboy:transaction(ezk_pooler, fun(EzkWorker) ->
%%       ok = gen_server:call(EzkWorker, {takeover, WorkerPid})
%%    end),

   wpool:call(PoolName, {takeover, WorkerPid}, next_worker, infinity),

   lets:delete_from_lists(monitored_pids, [?MODULE], WorkerPid),

   {noreply, State};
handle_info(stop, State) ->
   {stop, normal, State};
handle_info(_Info, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
   ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
   {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
maybe_new_watch(EzkWorker) ->
   {monitors, Monitors} = erlang:process_info(self(), monitors),
   Monitored = [M || {process, M} <- Monitors],
   new_watch(lists:member(EzkWorker, Monitored), EzkWorker).
new_watch(false, WorkerPid) ->
   lets:insert_list(monitored_pids, ?MODULE, WorkerPid),
   erlang:monitor(process, WorkerPid);
new_watch(true, _) ->
   ok.