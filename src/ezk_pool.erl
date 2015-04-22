%% Copyright LineMetrics 2013
-module(ezk_pool).
-author("Alexander Minichmair").

-include("ezk_pool.hrl").
%% API
-export([write/2, delete/1, delete_all/1, read/1, read_watch/2, read_watch/3]).

-export([start/0]).

%% start the app
start() ->
   ok = application:start(crypto),
   ok = application:start(ezk),
   ok = application:start(poolboy),
   ok = application:start(ezk_pool).


%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%

write(Path, Data) when is_binary(Data) ->
   poolboy:transaction(?EZK_POOL_NAME, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {write_data, path(Path), Data})
   end).

delete(Path) ->
   poolboy:transaction(?EZK_POOL_NAME, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {delete_node, path(Path)})
   end).

delete_all(Path) ->
   poolboy:transaction(?EZK_POOL_NAME, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {delete_node_all, path(Path)})
   end).

read(Path) ->
   poolboy:transaction(?EZK_POOL_NAME, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {get, path(Path)})
   end).

read_watch(Path, WatchName) ->
   read_watch(Path, self(), WatchName).

read_watch(Path, Watcher, WatchName) ->
   poolboy:transaction(?EZK_POOL_NAME, fun(EzkWorker) ->
      gen_server:call(EzkWorker, {get_watch, path(Path), Watcher, WatchName})
   end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal                                              %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
path(P) when is_binary(P) -> binary_to_list(P);
path(P) when is_list(P) -> P.