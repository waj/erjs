-module(erjs_heap).
-export([new/0, new_object/1, set_object_field/4, get_object_field/3, get_object_as_list/3, unset_object_field/3]).

new() ->
  dict:new().

new_object(Heap) ->
  Ref = make_ref(),
  Heap2 = dict:store(Ref, dict:new(), Heap),
  {Ref, Heap2}.

set_object_field(Ref, Field, Value, Heap) ->
  Object = dict:fetch(Ref, Heap),
  NewObject = dict:store(to_binary(Field), Value, Object),
  dict:store(Ref, NewObject, Heap).

get_object_as_list(Ref, Heap, Deep) ->
  Object = dict:fetch(Ref, Heap),
  List = dict:to_list(Object),
  case Deep of
    true ->
      [
        if
          is_reference(Value) -> {Field, get_object_as_list(Value, Heap, true)};
          true -> {Field, Value}
        end
      || {Field, Value} <- List];
    _ -> List
  end.

get_object_field(Ref, Field, Heap) ->
  Object = dict:fetch(Ref, Heap),
  case dict:find(to_binary(Field), Object) of
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
  dict:erase(to_binary(Name), Object).

to_binary(Name) when is_binary(Name) -> Name;
to_binary(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
to_binary(Name) when is_list(Name) -> iolist_to_binary(Name).
