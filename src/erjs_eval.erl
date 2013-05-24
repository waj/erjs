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
    _ when is_function(Value) -> "function";
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

run({apply, {identifier, _, Name}, {'(', Exps}}, C) ->
  {Args, C2} = lists:foldl(fun(Exp, {AIn, CIn}) ->
    {A, COut} = run(Exp, CIn),
    {AIn ++ [A], COut}
  end, {[], C}, Exps),

  case erjs_object:get(Name, C) of
    undefined ->
      erjs_builtins:run(Name, Args, C);
    Fun ->
      {erlang:apply(Fun, Args), C2}
  end;

run({integer, _, Value}, C) -> {Value, C};
run({float, _, Value}, C) -> {Value, C};
run({string, _, Value}, C) -> {Value, C}.

op('+', V1, V2) when is_list(V1) -> V1 ++ to_list(V2);
op('+', V1, V2) when is_list(V2) -> to_list(V1) ++ V2;
op('+', V1, V2) -> V1 + V2;
op('-', V1, V2) -> V1 - V2;
op('*', V1, V2) -> V1 * V2;

op('&&', V1, V2) ->
  case to_bool(V1) of
    true -> V2;
    _ -> V1
  end;
op('||', V1, V2) ->
  case to_bool(V1) of
    true -> V1;
    _ -> V2
  end;

op(Op, V1, V2) ->
  {X1, X2} = convert_for_comp(V1, V2, false),
  case Op of
    '<' -> X1 < X2;
    '<=' -> X1 =< X2;
    '>' -> X1 > X2;
    '>=' -> X1 >= X2;
    '==' -> X1 == X2;
    '!=' -> X1 /= X2;
    _ -> throw({badop, Op})
  end.

convert_for_comp(V1, V2, S) when is_list(V1), is_integer(V2) ->
  N = try list_to_integer(V1)  of
    X -> X
  catch
    _:_ -> V1
  end,
  set_for_comp(N, V2, S);

convert_for_comp(V1, V2, false) ->
  convert_for_comp(V2, V1, true);

convert_for_comp(V1, V2, true) -> {V2, V1}.

set_for_comp(V1, V2, false) -> {V1, V2};
set_for_comp(V1, V2, true) -> {V2, V1}.

to_list(X) when is_list(X) -> X;
to_list(X) when is_integer(X) -> integer_to_list(X).

to_bool(undefined) -> false;
to_bool(null) -> false;
to_bool(false) -> false;
to_bool(_) -> true.
