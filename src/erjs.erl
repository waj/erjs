-module(erjs).
-export([eval/1]).

eval(Data) ->
  {ok, Cmds} = erlyjs_compiler:parse(Data),
  erjs_eval:run(Cmds, dict:new()).
