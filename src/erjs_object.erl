-module(erjs_object).
-export([new/0, new/1, get/2, set/3, unset/2, to_list/1]).

new() ->
  dict:new().

new(Data) ->
  Object = new(),
  new(Data, Object).

new([], Object) -> Object;
new([{Name, Value} | Rest], Object) ->
  NewObject = set(Name, Value, Object),
  new(Rest, NewObject).

set(Name, Value, Object) ->
  dict:store(Name, Value, Object).

get(Name, Object) ->
  case dict:find(Name, Object) of
    {ok, Value} -> Value;
    _ -> undefined
  end.

unset([], Object) -> Object;
unset([Name | Rest], Object) ->
  unset(Rest, unset(Name, Object));
unset(Name, Object) ->
  dict:erase(Name, Object).

to_list(Object) ->
  dict:to_list(Object).