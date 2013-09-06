-module(erjs_test).
-include_lib("eunit/include/eunit.hrl").

cases() -> [
  {"123", 123, []},
  {"foo", undefined, []},
  {"a = 123", 123, [{<<"a">>, 123}]},
  {"a = 1; b = 2", 2, [{<<"a">>, 1}, {<<"b">>, 2}]},
  {"a = 1; b = 2; a + b", 3, [{<<"a">>, 1}, {<<"b">>, 2}]},
  {"2 + 3", 5, []},
  {"2 - 3", -1, []},
  {"2+3", 5, []},
  {"2.5", 2.5, []},
  {"1.2 + 2.3", 3.5, []},
  {"a = 1; a += 2; a", 3, [{<<"a">>, 3}]},
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
  {"foo = 1; foo ? foo : 2", 1, [{<<"foo">>, 1}]},
  {"if (1 > 2) { a = 123 }", undefined, []},
  {"if (1 < 2) { a = 123 }", undefined, [{<<"a">>,123}]},
  {"if (2 < 1) { a = 123 } else { a = 321 }", undefined, [{<<"a">>,321}]},
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
  {"'5' < 10", true, []},
  {"parseInt('123')", 123, []},
  {"parseFloat('2.5')", 2.5, []},
  {"5 * 4", 20, []},
  {"foo = {}; typeof(foo)", "object", [{<<"foo">>, []}]},
  {"foo = {}; foo['x'] = 123", 123, [{<<"foo">>, [{<<"x">>, 123}]}]},
  {"foo = {}; foo['x'] = 123; foo['x']", 123, [{<<"foo">>, [{<<"x">>, 123}]}]},
  {"foo = {}; foo.x = 123", 123, [{<<"foo">>, [{<<"x">>, 123}]}]},
  {"foo = {}; foo.x = 123; foo.x", 123, [{<<"foo">>, [{<<"x">>, 123}]}]},
  {"foo = {}; foo.x = 123; foo['x']", 123, [{<<"foo">>, [{<<"x">>, 123}]}]}
].

cases_with_state() -> [
  {[{foo, fun() -> 123 end}], "foo()", 123, []},
  {[{foo, fun(A, B) -> A + B end}], "foo(4, 5)", 9, []},
  {[{foo, fun(A, B) -> A + B end}], "foo(a = 4, b = 5)", 9, [{<<"a">>, 4}, {<<"b">>, 5}]},
  {[{foo, fun() -> ok end}], "typeof(foo)", "function", []}
].

js_test_() ->
  [{Code,
    fun() ->
      {Value, State} = erjs:eval(Code),
      ?assertEqual(ExpectedValue, Value),
      ?assertEqual(ExpectedState, lists:sort(erjs_context:to_list(State)))
    end}
  || {Code, ExpectedValue, ExpectedState} <- cases()].

js_with_state_test_() ->
  [{Code,
    fun() ->
      Context = erjs_context:new(Initial),
      {Value, State} = erjs:eval(Code, Context),
      ?assertEqual(ExpectedValue, Value),
      ?assertEqual(ExpectedState, lists:sort(remove_functions(erjs_context:to_list(State))))
    end}
  || {Initial, Code, ExpectedValue, ExpectedState} <- cases_with_state()].

hash_new_test() ->
  {Value, State} = erjs:eval("foo = {}"),
  ?assert(is_reference(Value)),
  ?assertEqual(Value, erjs_context:get(foo, State)).

object_new_test() ->
  {Value, State} = erjs:eval("foo = new Object()"),
  ?assert(is_reference(Value)),
  ?assertEqual(Value, erjs_context:get(foo, State)).

remove_functions([]) -> [];
remove_functions([{_, Value} | Rest]) when is_function(Value) -> remove_functions(Rest);
remove_functions([Item | Rest]) -> [Item | remove_functions(Rest)].

context_new_empty_test() ->
  Context = erjs_context:new(),
  ?assertEqual([], erjs_context:to_list(Context)).

context_new_test() ->
  Context = erjs_context:new([{a, 1}, {b, 2}]),
  ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}], lists:sort(erjs_context:to_list(Context))).

context_eval_test() ->
  Context = erjs_context:new([{a, 5}]),
  {_, Context2} = erjs:eval("a += 5", Context),
  ?assertEqual([{<<"a">>, 10}], erjs_context:to_list(Context2)).

context_unset_test() ->
  Context = erjs_context:new([{a, 1}, {b, 2}]),
  Context2 = erjs_context:unset(a, Context),
  Context3 = erjs_context:unset(c, Context2),
  ?assertEqual([{<<"b">>, 2}], erjs_context:to_list(Context3)).

context_unset_many_test() ->
  Context = erjs_context:new([{a, 1}, {b, 2}, {c, 3}]),
  Context2 = erjs_context:unset([a, b], Context),
  ?assertEqual([{<<"c">>, 3}], erjs_context:to_list(Context2)).
