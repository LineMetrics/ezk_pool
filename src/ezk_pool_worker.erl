%% Copyright LineMetrics 2015
-module(ezk_pool_worker).
-author("Alexander Minichmair").

-behaviour(gen_server).

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

-record(state, {conn}).

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
   {ok, EPid} = ezk:start_connection(),
   {ok, #state{conn = EPid}}.

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
   Res = ezk:get(Conn, Path, Watcher, WatchName),
   {reply, Res, State};
handle_call({ezk, EzkFun, Args}, _From, #state{conn = Conn} = State) ->
   Res = erlang:apply(ezk, EzkFun, [Conn|Args]),
   {reply, Res, State};
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
terminate(Reason, #state{conn = Conn}) ->
   ezk:end_connection(Conn, Reason),
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
