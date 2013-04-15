-module(erjs_eval).
-export([run/2]).

run([], C) -> {undefined, C};
run([S], C) -> run(S, C);
run([S|Rest], C) ->
  {_,C2} = run(S, C),
  run(Rest, C2);

run({identifier, _, true}, C) -> {true, C};
run({identifier, _, false}, C) -> {false, C};
run({identifier, _, null}, C) -> {null, C};

run({identifier, _, Name}, C) ->
  Value = erjs_object:get(Name, C),
  {Value, C};

run({assign, {'=', _}, {identifier, _, Name}, Exp}, C) ->
  {Value, C2} = run(Exp, C),
  C3 = erjs_object:set(Name, Value, C2),
  {Value, C3};

run({assign, {'+=', N}, Target, Exp}, C) ->
  Value = {op, {'+', N}, Target, Exp},
  run({assign, {'=', N}, Target, Value}, C);

run({op, {Op, _}, E1, E2}, C) ->
  {V1, C1} = run(E1, C),
  {V2, C2} = run(E2, C1),
  {op(Op, V1, V2), C2};

run({op, 'cond', Expr, Then, Else}, C) ->
  {Cond, C1} = run(Expr, C),
  Next = case to_bool(Cond) of
    true -> Then;
    _ -> Else
  end,
  run(Next, C1);

run({op, {typeof, _}, Expr}, C) ->
  {Value, C1} = run(Expr, C),
  Type = case Value of
    true -> "boolean";
    false -> "boolean";
    undefined -> "undefined";
    _ when is_number(Value) -> "number";
    _ when is_list(Value) -> "string";
    _ -> "object"
  end,
  {Type, C1};

run({'if', Expr, Then}, C) ->
  run({ifelse, Expr, Then, []}, C);

run({ifelse, Expr, Then, Else}, C) ->
  {Cond, C1} = run(Expr, C),
  Next = case to_bool(Cond) of
    true -> Then;
    _ -> Else
  end,
  {_, C2} = run(Next, C1),
  {undefined, C2};


run({integer, _, Value}, C) -> {Value, C};
run({string, _, Value}, C) -> {Value, C}.

op('+', V1, V2) when is_list(V1) -> V1 ++ to_list(V2);
op('+', V1, V2) when is_list(V2) -> to_list(V1) ++ V2;

op('+', V1, V2) -> V1 + V2;
op('<', V1, V2) -> V1 < V2;
op('<=', V1, V2) -> V1 =< V2;
op('>', V1, V2) -> V1 > V2;
op('>=', V1, V2) -> V1 >= V2;
op('==', V1, V2) -> V1 == V2;
op('!=', V1, V2) -> V1 /= V2;
op('&&', V1, V2) ->
  case to_bool(V1) of
    true -> V2;
    _ -> V1
  end;
op('||', V1, V2) ->
  case to_bool(V1) of
    true -> V1;
    _ -> V2
  end.

to_list(X) when is_list(X) -> X;
to_list(X) when is_integer(X) -> integer_to_list(X).

to_bool(undefined) -> false;
to_bool(null) -> false;
to_bool(false) -> false;
to_bool(_) -> true.
