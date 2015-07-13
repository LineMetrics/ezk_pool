%% Copyright LineMetrics 2013
-author("Alexander Minichmair").

%% -define(DEBUG, lists:member("debug", init:get_plain_arguments())).
-define(
LOG,
fun(Template, Values) when is_list(Values) ->
   D = false,%not is_list(Values),
   if D == true  -> io:format("~n~n:::::::  ~p  ~p ~n" ++ Template, [self() | [?MODULE | Values]]);
      true        -> ok
   end
end
).
