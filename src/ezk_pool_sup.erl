-module(ezk_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
   Processes = [
   % ETS Table Manager =
      {ets_manager,
         {ets_manager, start_link, []},
         permanent, 5000, worker, dynamic},

   % Watcher-Pool-Watcher =
      {pool_watcher,
         {pool_watcher, start_link, []},
         permanent, 5000, worker, dynamic},

      % Watcher-Pool-Watcher =
      {client_watcher,
         {client_watcher, start_link, []},
         permanent, 5000, worker, dynamic}
   ],

   Pools = application:get_all_env(ezk_pool),
   Pools1 = proplists:delete(included_applications, Pools),
   lists:foreach(
      fun ({PoolName, PoolArgs}) ->
         io:format("~ncreate pool: ~p with Args: ~p~n",[PoolName, PoolArgs]),
         wpool:start_sup_pool(PoolName, PoolArgs)
%%          ezk_pool:child_spec(PoolName, PoolArgs)
      end,
      Pools1
   ),

   {ok, { {one_for_one, 15, 15}, Processes} }.
