-module(erjs_builtins).
-export([run/3, datetime_new/1, datetime_invoke/2]).

run(parseInt, [Str], C) -> {list_to_integer(Str), C};
run(parseFloat, [Str], C) -> {list_to_float(Str), C};

run(_, _, _) ->
  throw(undefined).

datetime_new([]) ->
  calendar:universal_time();
datetime_new([MS]) when is_integer(MS) ->
  Secs = ((MS div 1000) + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})),
  calendar:gregorian_seconds_to_datetime(Secs).

datetime_invoke({{_, _, D}, _}, getDate) -> D;
datetime_invoke({{_, M, _}, _}, getMonth) -> M - 1;
datetime_invoke({{Y, _, _}, _}, getFullYear) -> Y;
datetime_invoke(DateTime, getTime) ->
  (calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})) * 1000;
datetime_invoke(_, _) -> throw(undefined).
