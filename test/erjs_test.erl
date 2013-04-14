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
  {"if (2 < 1) { a = 123 } else { a = 321 }", undefined, [{a,321}]}
].

js_test_() ->
  [
    fun() ->
      {Value, State} = erjs:eval(Code),
      ?assertEqual(ExpectedValue, Value),
      ?assertEqual(ExpectedState, lists:sort(dict:to_list(State)))
    end
  || {Code, ExpectedValue, ExpectedState} <- cases()].