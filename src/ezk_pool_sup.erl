-module(ezk_pool_sup).

-behaviour(supervisor).

-include("ezk_pool.hrl").
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
   Pools = application:get_all_env(ezk_pool),
   Pools1 = proplists:delete(included_applications, Pools),
   PoolSpec = lists:map(
      fun ({PoolName, PoolArgs}) ->
         ezk_pool:child_spec(PoolName, PoolArgs)
      end,
      Pools1
   ),
   {ok, { {one_for_one, 15, 15}, PoolSpec} }.
