-module(erjs_builtins).
-export([run/3]).

run(parseInt, [Str], C) -> {list_to_integer(Str), C};
run(parseFloat, [Str], C) -> {list_to_float(Str), C};

run(_, _, _) ->
  throw(undefined).