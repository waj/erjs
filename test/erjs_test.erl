-module(erjs_test).
-include_lib("eunit/include/eunit.hrl").

cases() -> [
  {"123", 123, []},
  {"foo", undefined, []},
  {"a = 123", 123, [{a, 123}]},
  {"a = 1; b = 2", 2, [{a, 1}, {b, 2}]},
  {"a = 1; b = 2; a + b", 3, [{a, 1}, {b, 2}]},
  {"2 + 3", 5, []},
  {"2 - 3", -1, []},
  {"2+3", 5, []},
  {"2.5", 2.5, []},
  {"1.2 + 2.3", 3.5, []},
  {"a = 1; a += 2; a", 3, [{a, 3}]},
  {"true", true, []},
  {"false", false, []},
  {"null", null, []},
  {"1 == 1", true, []},
  {"1 == 2", false, []},
  {"1 != 2", true, []},
  {"1 < 2", true, []},
  {"5 <= 2", false, []},
  {"\"foo\"", "foo", []},
  {"'hello' + 1", "hello1", []},
  {"1 + 'hello'", "1hello", []},
  {"1 < 2 ? 3 : 4", 3, []},
  {"foo ? foo : 1", 1, []},
  {"foo = 1; foo ? foo : 2", 1, [{foo, 1}]},
  {"if (1 > 2) { a = 123 }", undefined, []},
  {"if (1 < 2) { a = 123 }", undefined, [{a,123}]},
  {"if (2 < 1) { a = 123 } else { a = 321 }", undefined, [{a,321}]},
  {"typeof(1)", "number", []},
  {"typeof('foo')", "string", []},
  {"typeof(a)", "undefined", []},
  {"typeof(true)", "boolean", []},
  {"typeof(false)", "boolean", []},
  {"typeof(null)", "object", []},
  {"true && true", true, []},
  {"null && 123", null, []},
  {"false || true", true, []},
  {"'1' == 1", true, []},
  {"1 == '1'", true, []},
  {"'5' < 10", true, []}
].

cases_with_state() -> [
  {[{foo, fun() -> 123 end}], "foo()", 123, []},
  {[{foo, fun(A, B) -> A + B end}], "foo(4, 5)", 9, []},
  {[{foo, fun(A, B) -> A + B end}], "foo(a = 4, b = 5)", 9, [{a, 4}, {b, 5}]},
  {[{foo, fun() -> ok end}], "typeof(foo)", "function", []}
].

js_test_() ->
  [{Code,
    fun() ->
      {Value, State} = erjs:eval(Code),
      ?assertEqual(ExpectedValue, Value),
      ?assertEqual(ExpectedState, lists:sort(erjs_object:to_list(State)))
    end}
  || {Code, ExpectedValue, ExpectedState} <- cases()].

js_with_state_test_() ->
  [{Code,
    fun() ->
      Global = erjs_object:new(Initial),
      {Value, State} = erjs:eval(Code, Global),
      ?assertEqual(ExpectedValue, Value),
      ?assertEqual(ExpectedState, lists:sort(remove_functions(erjs_object:to_list(State))))
    end}
  || {Initial, Code, ExpectedValue, ExpectedState} <- cases_with_state()].

remove_functions([]) -> [];
remove_functions([{_, Value} | Rest]) when is_function(Value) -> remove_functions(Rest);
remove_functions([Item | Rest]) -> [Item | remove_functions(Rest)].

global_new_empty_test() ->
  Global = erjs_object:new(),
  ?assertEqual([], erjs_object:to_list(Global)).

global_new_test() ->
  Global = erjs_object:new([{a, 1}, {b, 2}]),
  ?assertEqual([{a, 1}, {b, 2}], lists:sort(erjs_object:to_list(Global))).

global_eval_test() ->
  Global = erjs_object:new([{a, 5}]),
  {_, Global2} = erjs:eval("a += 5", Global),
  ?assertEqual([{a, 10}], erjs_object:to_list(Global2)).

global_unset_test() ->
  Global = erjs_object:new([{a, 1}, {b, 2}]),
  Global2 = erjs_object:unset(a, Global),
  Global3 = erjs_object:unset(c, Global2),
  ?assertEqual([{b, 2}], erjs_object:to_list(Global3)).

global_unset_many_test() ->
  Global = erjs_object:new([{a, 1}, {b, 2}, {c, 3}]),
  Global2 = erjs_object:unset([a, b], Global),
  ?assertEqual([{c, 3}], erjs_object:to_list(Global2)).

