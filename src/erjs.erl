-module(erjs).
-export([eval/1, eval/2]).

eval(Data) ->
  Context = erjs_context:new(),
  eval(Data, Context).

eval(Data, Context) ->
  {ok, Cmds} = erlyjs_compiler:parse(Data),
  erjs_eval:run(Cmds, Context).
