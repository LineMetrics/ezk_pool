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
   Ps =
   %% ezk server pool
   [
   poolboy:child_spec(?EZK_POOL_NAME,
      [
         {name, {local, ?EZK_POOL_NAME}},
         {size, 5},
         {max_overflow, 10},
         {worker_module, ezk_pool_worker},
         {strategy, fifo}
      ]
      , [])
   ]
   ,
   {ok, { {one_for_one, 5, 10}, Ps} }.
