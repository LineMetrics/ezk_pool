%% Copyright LineMetrics 2013
-author("Alexander Minichmair").

-define(
LOG,
fun(Template, Values) when is_list(Values) ->
   io:format("~n~n:::::::  ~p  ~p ~n" ++ Template, [self()| [?MODULE | Values]])
end
).
