%% Copyright LineMetrics 2013
-module(ezk_pool).
-author("Alexander Minichmair").

%% API
-export([new/2, child_spec/2]).
-export([write/3, delete/2, delete_all/2, read/2, read_watch/3, read_watch/4]).

-export([start/0]).

%% start the app
start() ->
   application:start(crypto),
   lager:start(),
   application:start(ezk),
   application:start(poolboy),
   ok = application:start(ezk_pool).


%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%
%%%% pool handling
new(PoolName, PoolBoyOptions) ->
   supervisor:start_child(ezk_pool_sup, child_spec(PoolName, PoolBoyOptions)).

child_spec(PoolName, PoolArgs) ->
   PoolArgs1 = [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, ezk_pool_worker} | PoolArgs],
   poolboy:child_spec(PoolName, PoolArgs1)
.

%%%%%%%%%% worker
write(PoolName, Path, Data) when is_binary(Data) ->
   poolboy:transaction(PoolName, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {write_data, path(Path), Data})
   end).

delete(PoolName, Path) ->
   poolboy:transaction(PoolName, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {delete_node, path(Path)})
   end).

delete_all(PoolName, Path) ->
   poolboy:transaction(PoolName, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {delete_node_all, path(Path)})
   end).

read(PoolName, Path) ->
   poolboy:transaction(PoolName, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {get, path(Path)})
   end).

read_watch(PoolName, Path, WatchName) ->
   read_watch(PoolName, Path, self(), WatchName).

read_watch(PoolName, Path, Watcher, WatchName) ->
   poolboy:transaction(PoolName, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {get_watch, path(Path), Watcher, WatchName})
   end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal                                              %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path(P) when is_binary(P) -> binary_to_list(P);
path(P) when is_list(P) -> P.