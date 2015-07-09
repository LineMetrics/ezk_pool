%% Copyright LineMetrics 2015
%% ets value-list helper functions
-module(lets).
-author("Alexander Minichmair").

%% API
-export([insert_list/3, read_list/2, delete_from_lists/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper funcs for handling list values in ets key=value schema
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handles unique entries of unordered list stored in ets with arbitrary key
%%
%% members of the value-lists are unique
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
%% if the list is empty, we delete the value-list from the ets table
delete_from_lists(TableName, KeyList, Value) ->
   [delete_from_list(TableName, Key, Value) || Key <- KeyList]
.


delete_from_list(TableName, Key, Value) ->
   delete_from_list(TableName, Key, read_list(TableName, Key), Value)
.

delete_from_list(TableName, Key, [], _) ->
   ets:delete(TableName, Key);
delete_from_list(TableName, Key, [Val], Val) ->
   ets:delete(TableName, Key);
delete_from_list(T, K, List, Val) when is_list(List) ->
   delete_from_list(lists:member(Val, List), T, K, List, Val)
.
delete_from_list(true, T, K, List, Val) ->
   ets:insert(T, {K, lists:delete(Val, List)});
delete_from_list(false, _, _, _, _) ->
   true
.
