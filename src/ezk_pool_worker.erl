%% Copyright LineMetrics 2015
-module(ezk_pool_worker).
-author("Alexander Minichmair").

-behaviour(gen_server).

-include("../include/ezk_pool.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {conn, watches :: set(), callback_url}).

%% -record(db_watcher_pid, {pid, path, worker_pid}).
%% -record(db_watcher_base, {basepath, pid, worker_pid}).
%% -record(db_watcher_path, {path, pid, handler_pid}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
   gen_server:start_link(?MODULE, Args, []).

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
init(_Args) ->
%%    ?LOG("starting ezk_pool_worker",[]),
   EPid = init_conn(),
   Set = sets:new(),
   {ok, #state{conn = EPid, watches = Set}}.

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
%% write some data to a zk-node, ensure the node exists
handle_call({write_data, Path, Data}, _From, #state{conn = Conn} = State)
                                                          when is_list(Path) andalso is_binary(Data) ->
   ezk:ensure_path(Conn, Path),
   Res = ezk:set(Conn, Path, Data),
   {reply, Res, State};
%% delete a node, if it has no children
handle_call({delete_node, Path}, _From, #state{conn = Conn} = State) when is_list(Path) ->
   Res = ezk:delete(Conn, Path),
   {reply, Res, State};
%% delete a node and all its children
handle_call({delete_node_all, Path}, _From, #state{conn = Conn} = State) when is_list(Path) ->
   Res = ezk:delete_all(Conn, Path),
   {reply, Res, State};
%% read a nodes content
handle_call({get, Path}, _From, #state{conn = Conn} = State) when is_list(Path) ->
   Res = ezk:get(Conn, Path),
   {reply, Res, State};
%% read a nodes content and set a watch on the path
handle_call({get_watch, Path, Watcher, WatchName}, _From, #state{conn = Conn} = State) when is_list(Path) ->
   {ok, Res} = watch_path(Conn, Path, Watcher, WatchName),
   {reply, Res, State};
   %%
%%
handle_call({setup_get_watch, Path, WatchName}, {ClientPid, _Tag}, #state{conn = Conn} = State) ->

   ?LOG("~nsetup_get_watch with ~p~n",[{Path, WatchName}]),
   gen_server:cast(pool_watcher, {new_watch, self()}),
   gen_server:cast(client_watcher, {new_watch, ClientPid}),
   %% check for valid watcher process
   {Data, NewState} =
   case ets:lookup(path_watcher, Path) of
      [{Path, WatcherPid}] ->
         %% make sure existing worker is alive
         case is_process_alive(WatcherPid) of
            true  ->
                     update_client_path(Path, ClientPid, WatchName),
                     {ok, {Data0,_R}} = ezk:get(Conn, Path),
                     ?LOG("living watcher found for path (~p)  no need to set watch~n",[Path]),
                     {Data0, State};

            false -> {ok,Data1}  = start_watch(Conn, Path, ClientPid, WatchName), {Data1, State}
         end;
      _Nope                ->
         {ok, Data2} = start_watch(Conn, Path, ClientPid, WatchName),
         {Data2, State}
   end,
   {reply, {ok,Data}, NewState}
;
%% takeover watches form an old process
handle_call({takeover, OldPid}, _From, State) ->
   ?LOG("~nTAKEOVER ALL WATCHES FROM PID: ~p to pid: ~p~n",[OldPid, self()]),
   rewatch_all(State#state.conn, OldPid),
   {reply, ok, State};
handle_call({ezk, EzkFun, Args}, _From, #state{conn = Conn} = State) ->
   Res = erlang:apply(ezk, EzkFun, [Conn|Args]),
   {reply, Res, State};
handle_call(_Request, _From, State) ->
   ?LOG("unhandled call in ezk_pool_worker~n"),
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
handle_cast(_Request, State) ->
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
%%%%%%%%%%%%%%%%%%%%%%%%
%% zookeeper connection is down, reconnect and rewatch all basepaths
handle_info({'DOWN', _MonitorRef, process, Object, _Info}, State) ->
   %% start a new ezk connection
   ?LOG("Zookeeper Connection is 'DOWN' (monitored process(ezk)) ~p", [Object]),
   NewConnPid = init_conn(),
   NewState = State#state{conn  = NewConnPid},
   rewatch_all(NewConnPid, self()),
   {noreply, NewState};

%% watch message data_changed
handle_info(M={_WatchName, {Path, data_changed, _SyncCon}}, State) ->
   ?LOG("Watcher::: ~p~n data has changed for path ~p (type: ~p)",[M, Path, data_changed]),
   %% get_data and rewatch
   %% inform interested pids
   maybe_rewatch(State#state.conn, Path),
   {noreply, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the zk-watch is lost
%% eventually re-watch
handle_info({watchlost, WM, {Type, Path}}, #state{conn = ConnectionPid}=State) ->
   ?LOG("Zookeeper-Watch lost for path ~p, Type: ~p, WatchMessage: ~p", [Path, Type, WM]),

   %% if the connection process is dead, then we do nothing, because a 'DOWN' message will arrive
   %% soon after this one and a new connection has to be established anyways

   case is_process_alive(ConnectionPid) of
      true ->
               maybe_rewatch(ConnectionPid, Path);

      false -> %% do nothing
               ok
   end,
   {noreply, State};
%% zookeeper node deleted !!
handle_info(_M = {WatchName, {Path, node_deleted, _SyncCon}}, State) ->
   ?LOG("zookeeper node deleted (~p) ",[Path]),
   %% inform clients
   PathClients = lets:read_list(path_clients, Path),
   [Pid ! {node_deleted, WatchName, Path} || Pid <- PathClients],
   %% delete from client tables
   ets:delete(path_clients, Path),
   lets:delete_from_lists(client_paths, PathClients, Path),
   %%
   %% delete path watcher entry (not a list)
   ets:delete(path_watcher, Path),
   %% get all watchers for this path ..
   PathsAll = lets:read_list(watcher_paths, self()),
   %% .. and write back the list with Path excluded
   ets:insert(watcher_paths, {self(), lists:delete(Path, PathsAll)}),

   {noreply, State};
handle_info(timeout, State) ->
   ?LOG("timeout in ezk_pool_worker: ~p",[self()]),
   {stop, timeout, State};
handle_info(_Info, State) ->
   ?LOG("unhandled INFO in ezk_pool_worker: ~p",[_Info]),
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
terminate(Reason, #state{conn = Conn}) ->
   catch ezk:end_connection(Conn, Reason),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%===================================================================
%%% Internal functions
%%%===================================================================
init_conn() ->
   {ok, EPid} = ezk:start_connection(),
   _Monitor = erlang:monitor(process, EPid),
   EPid.

%% if we do not have any clients left for the path, do not start the watch
maybe_rewatch(Conn, Path) ->
   Clients = lets:read_list(path_clients, Path),
   IsWatcher = lists:member(Path, lets:read_list(watcher_paths, self())),
   case {Clients, IsWatcher} of
      {_C, _Is} when Clients =:= [] orelse IsWatcher =:= false -> %% delete
         ?LOG("no more clients left for path or self() is not watcher: ~p -> do not restart watch",[Path]),
         ets:delete(path_watcher, Path),
         PathsAll = lets:read_list(watcher_paths, self()),
         ets:insert(watcher_paths, {self(), lists:delete(Path, PathsAll)}),
         ok;
      {L, true} ->
         {ok, NewData} = start_watch(Conn, Path),
         data_changed(Path, L, NewData)
   end.

%% start a new watch and store in client watches
start_watch(Conn, Path, ClientPid, WatchName) ->
   {ok, Data} = start_watch(Conn, Path),
   update_client_path(Path, ClientPid, WatchName),
   {ok, Data}.
%% just start/restart a watch,
%% not updating client ets-tables as everything else should stay the same
start_watch(Conn, Path) ->
   ?LOG("start new watch on path: ~p",[Path]),
   ezk:ensure_path(Conn, Path),
   {ok, Data1} = watch_path(Conn, Path, self()),
   true = ets:insert(path_watcher,{Path, self()}),
   true = lets:insert_list(watcher_paths, self(), Path),
   {ok, Data1}.

%% inform pids about new path-data
data_changed(_Path, [], _NewData) ->
   ok;
data_changed(Path, Clients, NewData) ->
   [(Pid ! {data_changed, watch_name(Pid, Path), Path, NewData}) || Pid <- Clients].

%% rewatch and send all data changed for all watches of a watcher-pid
%% as a result of this action, the current (self()) process is then in charge of all
%% the watches held by Pid so far
rewatch_all(Conn, Pid) ->
   PathList = lets:read_list(watcher_paths, Pid),
   ?LOG("rewatch all : ~p",[PathList]),
   Inform = fun(IPath) -> maybe_rewatch(Conn, IPath) end,
   ok = lists:foreach(Inform, PathList).

%% watch a node for changes on zookeeper
watch_path(ConnPid, Path, Watcher) ->
   watch_path(ConnPid, Path, Watcher, Path)
   .
watch_path(ConnPid, Path, Watcher, WatchMessage) ->
   ?LOG("watch path (~p, ~p, ~p)",[ConnPid, Path, Watcher]),
   {ok, {Data, {getdata, _Czxid, _Mzxid, _Pzxid, _Ctime, _Mtime, _Dataversion,
      _Datalength, _Number_children, _Cversion, _Aclversion, _Ephe_owner}}}
      = ezk:get(ConnPid, Path, Watcher, WatchMessage),

   {ok, Data}.

%% lookup a watchname for a ClientPid-Path combination
%% returns atom(watchname) or list(Path)
watch_name(Pid, Path) ->
   Names = lets:read_list(client_path_name, Pid),
   case proplists:get_value(Path, Names) of
      undefined -> Path;
      WatchName -> WatchName
   end.

update_client_path(Path, ClientPid, WatchName) ->
   true = lets:insert_list(path_clients, Path, ClientPid),
   true = lets:insert_list(client_paths, ClientPid, Path),
   true = lets:insert_list(client_path_name, ClientPid, {Path, WatchName}).

