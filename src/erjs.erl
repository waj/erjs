-module(erjs).
-export([eval/1, eval/2]).

eval(Data) ->
  Global = erjs_object:new(),
  eval(Data, Global).

eval(Data, Global) ->
  {ok, Cmds} = erlyjs_compiler:parse(Data),
  erjs_eval:run(Cmds, Global).
