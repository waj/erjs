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
  Value = erjs_context:get(Name, C),
  {Value, C};

run({ObjExp, {'[]', FieldExp}}, C) ->
  {Obj, C2} = run(ObjExp, C),
  {Field, C3} = run(FieldExp, C2),
  Value = erjs_context:get(Obj, Field, C3),
  {Value, C3};

run({assign, {'=', _}, {identifier, _, Name}, Exp}, C) ->
  {Value, C2} = run(Exp, C),
  C3 = erjs_context:set(Name, Value, C2),
  {Value, C3};

run({assign, {'=', _}, {ObjExp, {'[]', FieldExp}}, ValueExp}, C) ->
  {Obj, C2} = run(ObjExp, C),
  {Field, C3} = run(FieldExp, C2),
  {Value, C4} = run(ValueExp, C3),
  C5 = erjs_context:set(Obj, Field, Value, C4),
  {Value, C5};

run({assign, {'+=', N}, Target, Exp}, C) ->
  Value = {op, {'+', N}, Target, Exp},
  run({assign, {'=', N}, Target, Value}, C);

run({new, {identifier, _, 'Date'}, {'(', Exps}}, C) ->
  {Args, C2} = run_args(Exps, C),
  {erjs_builtins:datetime_new(Args), C2};

run({new, {identifier, _, 'Object'}, {'(', []}}, C) ->
  erjs_context:new_object(C);

run({'{', _}, C) ->
  erjs_context:new_object(C);

run({{Expr, [Method]}, {'(', []}}, C) ->
  {Obj, C2} = run(Expr, C),
  case is_datetime(Obj) of
    true -> {erjs_builtins:datetime_invoke(Obj, Method), C2};
    false -> throw(undefined)
  end;

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
  {Args, C2} = run_args(Exps, C),
  case erjs_context:get(Name, C2) of
    undefined ->
      erjs_builtins:run(Name, Args, C2);
    Fun ->
      {erlang:apply(Fun, Args), C2}
  end;

run({integer, _, Value}, C) -> {Value, C};
run({float, _, Value}, C) -> {Value, C};
run({string, _, Value}, C) -> {Value, C}.

run_args(Args, C) ->
  lists:foldl(fun(Exp, {AIn, CIn}) ->
    {A, COut} = run(Exp, CIn),
    {AIn ++ [A], COut}
  end, {[], C}, Args).

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

is_datetime({{Y, M, D},{HH, MM, SS}}) when is_number(Y), is_number(M), is_number(D), is_number(HH), is_number(MM), is_number(SS) -> true;
is_datetime(_) -> false.
