-module(erjs_context).
-export([new/0, new/1, set/3, set/4, get/2, get/3, get_object/2, to_list/1, unset/2, new_object/1]).

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
  set(Root, Name, Value, {Root, Heap}).

set(Obj, Name, Value, {Root, Heap}) ->
  Heap2 = erjs_heap:set_object_field(Obj, Name, Value, Heap),
  {Root, Heap2}.

get(Name, {Root, Heap}) ->
  get(Root, Name, {Root, Heap}).

get(Obj, Name, {_Root, Heap}) ->
  erjs_heap:get_object_field(Obj, Name, Heap).

unset(Name, {Root, Heap}) ->
  Heap2 = erjs_heap:unset_object_field(Root, Name, Heap),
  {Root, Heap2}.

new_object({Root, Heap}) ->
  {Ref, Heap2} = erjs_heap:new_object(Heap),
  {Ref, {Root, Heap2}}.

get_object(Obj, {_Root, Heap}) ->
  erjs_heap:get_object_as_list(Obj, Heap, false).

to_list({Root, Heap}) ->
  erjs_heap:get_object_as_list(Root, Heap, true).
