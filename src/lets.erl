%% Copyright LineMetrics 2015
%% ets value-list helper functions
-module(lets).
-author("Alexander Minichmair").

%% API
-export([]).
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper funcs for handling list values in ets key=value schema
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the scheme for the ets tables in ezk_pool is: {Key, Value}
%% Key is always a list (string) at the moment
%% in tables: watcher_paths, client_paths, path_clients the value is a list
%% and in table: path_watcher the value is the pool-workers pid
%%
%% members of the value-lists in the 3 named tables are unique
%%
%% this functions only cares about one thing: that values are lists
%% they work with every key-type that works with ets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% insert into value-list if not already member
insert_list(TableName, Key, NewListValue) ->
   List = read_list(TableName, Key),
   case lists:member(NewListValue, List) of
      true  -> true;
      false -> ets:insert(TableName, {Key, [NewListValue|List]})
   end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% read a value-list from an ets table with Key
%% returns either the found value-list or []
read_list(TableName, Key) ->
   case ets:lookup(TableName, Key) of
      [{Key, RList}] -> RList;
      [] -> []
   end.

%%%%%%%%%%%% delete %%%%%%%%%%%%%
%% delete entry (Value) from all lists referenced by KeyList-keys in the ets table TableName
delete_from_lists(TableName, KeyList, Value) ->
%%    [delete_from_list(TableName, Key, Value) || Key <- KeyList],
   F = fun(Key) ->
      delete_from_list(TableName, Key, Value),
      case read_list(TableName, Key) of
         [] -> %% delete whole entry
            ets:delete(TableName, Key);
         _ -> ok
      end
   end,
   lists:foreach(F, KeyList)
.

delete_from_list(TableName, Key, Value) ->
   delete_from_list(TableName, Key, read_list(TableName, Key), Value)
.

delete_from_list(_, _, [], _) ->
   true;
delete_from_list(T, K, L, V) when is_list(L) ->
   delete_from_list(lists:member(V, L), T, K, L, V)
.

delete_from_list(true, T, K, L, V) ->
   ets:insert(T, {K, lists:delete(V, L)});
delete_from_list(false, _, _, _, _) ->
   true.