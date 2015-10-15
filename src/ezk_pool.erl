%% Copyright LineMetrics 2013
-module(ezk_pool).
-author("Alexander Minichmair").

%% API
-export([setup_get_watch/3]).
-export([set/3, delete/2, delete_all/2, get/2, get_watch/3, get_watch/4, call/3]).

-export([start/0]).

%% start the app
start() ->
   application:ensure_all_started(?MODULE, permanent).


%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%
%%%% pool handling

%%%%%%%%%% worker
set(PoolName, Path, Data) when is_binary(Data) ->
   wpool:call(PoolName, {write_data, path(Path), Data}, next_worker, infinity).
%%    poolboy:transaction(PoolName, fun(EzkWorker) ->
%%       gen_server:call(EzkWorker, {write_data, path(Path), Data})
%%    end).

delete(PoolName, Path) ->
   wpool:call(PoolName, {delete_node, path(Path)}, next_worker, infinity).
%%    poolboy:transaction(PoolName, fun(EzkWorker) ->
%%       gen_server:call(EzkWorker, {delete_node, path(Path)})
%%    end).

delete_all(PoolName, Path) ->
   wpool:call(PoolName, {delete_node_all, path(Path)}, next_worker, infinity).
%%    poolboy:transaction(PoolName, fun(EzkWorker) ->
%%       gen_server:call(EzkWorker, {delete_node_all, path(Path)})
%%    end).

get(PoolName, Path) ->
   wpool:call(PoolName, {get, path(Path)}, next_worker, infinity).
%%    poolboy:transaction(PoolName, fun(EzkWorker) ->
%%       gen_server:call(EzkWorker, {get, path(Path)})
%%    end).

get_watch(PoolName, Path, WatchName) ->
   get_watch(PoolName, Path, self(), WatchName).

get_watch(PoolName, Path, Watcher, WatchName) ->
   wpool:call(PoolName, {get_watch, path(Path), Watcher, WatchName}, next_worker, infinity).
%%    poolboy:transaction(PoolName, fun(EzkWorker) ->
%%       gen_server:call(EzkWorker, {get_watch, path(Path), Watcher, WatchName})
%%    end).
setup_get_watch(PoolName, Path, WatchName) ->
   pool_watcher:set_pool_name(PoolName),
   wpool:call(PoolName, {setup_get_watch, path(Path), WatchName}, next_worker, infinity).

%%    poolboy:transaction(PoolName, fun(EzkWorker) ->
%%       gen_server:cast(pool_watcher, {new_watch, EzkWorker}),
%%       gen_server:call(EzkWorker, {setup_get_watch, path(Path), WatchName})
%%    end).
%% @doc Call any function from the ezk module with some Arguments
%% provide the Arguments-List without the Connection Pid
call(PoolName, FunctionName, Args) when is_atom(FunctionName) andalso is_list(Args) ->
   wpool:call(PoolName, {ezk, FunctionName, Args}, next_worker, infinity).
%%    poolboy:transaction(PoolName, fun(EzkWorker) ->
%%       gen_server:call(EzkWorker, {ezk, FunctionName, Args})
%%    end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal                                              %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path(P) when is_binary(P) -> binary_to_list(P);
path(P) when is_list(P) -> P.
