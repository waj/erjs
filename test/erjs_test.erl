-module(erjs_test).
-include_lib("eunit/include/eunit.hrl").

cases() -> [
  {"123", 123, []},
  {"foo", undefined, []},
  {"a = 123", 123, [{a, 123}]},
  {"a = 1; b = 2", 2, [{a, 1}, {b, 2}]},
  {"a = 1; b = 2; a + b", 3, [{a, 1}, {b, 2}]},
  {"2 + 3", 5, []},
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
  {"typeof(null)", "object", []}
].

js_test_() ->
  [
    fun() ->
      {Value, State} = erjs:eval(Code),
      ?assertEqual(ExpectedValue, Value),
      ?assertEqual(ExpectedState, lists:sort(erjs_object:to_list(State)))
    end
  || {Code, ExpectedValue, ExpectedState} <- cases()].

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

