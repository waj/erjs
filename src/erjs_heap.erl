-module(erjs_heap).
-export([new/0, new_object/1, set_object_field/4, get_object_field/3, get_object_as_list/2, unset_object_field/3]).

new() ->
  dict:new().

new_object(Heap) ->
  Ref = make_ref(),
  Heap2 = dict:store(Ref, dict:new(), Heap),
  {Ref, Heap2}.

set_object_field(Ref, Field, Value, Heap) ->
  Object = dict:fetch(Ref, Heap),
  NewObject = dict:store(Field, Value, Object),
  dict:store(Ref, NewObject, Heap).

get_object_as_list(Ref, Heap) ->
  Object = dict:fetch(Ref, Heap),
  dict:to_list(Object).

get_object_field(Ref, Field, Heap) ->
  Object = dict:fetch(Ref, Heap),
  case dict:find(Field, Object) of
    {ok, Value} -> Value;
    _ -> undefined
  end.

unset_object_field(Ref, Field, Heap) ->
  Object = dict:fetch(Ref, Heap),
  NewObject = unset(Field, Object),
  dict:store(Ref, NewObject, Heap).

unset([], Object) -> Object;
unset([Name | Rest], Object) ->
  unset(Rest, unset(Name, Object));
unset(Name, Object) ->
  dict:erase(Name, Object).

