-module(erjs_context).
-export([new/0, new/1, set/3, get/2, to_list/1, unset/2]).

new() ->
  Heap = erjs_heap:new(),
  erjs_heap:new_object(Heap).

new(Data) ->
  Context = new(),
  new(Data, Context).

new([], Context) -> Context;
new([{Name, Value} | Rest], Context) ->
  NewContext = set(Name, Value, Context),
  new(Rest, NewContext).

set(Name, Value, {Root, Heap}) ->
  Heap2 = erjs_heap:set_object_field(Root, Name, Value, Heap),
  {Root, Heap2}.

get(Name, {Root, Heap}) ->
  erjs_heap:get_object_field(Root, Name, Heap).

unset(Name, {Root, Heap}) ->
  Heap2 = erjs_heap:unset_object_field(Root, Name, Heap),
  {Root, Heap2}.

to_list({Root, Heap}) ->
  erjs_heap:get_object_as_list(Root, Heap).
