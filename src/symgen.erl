-module(symgen).
-export([main/1]).

main(File) ->
  Forms = forms:read(File ++ ".erl"),
  FiltForms = forms:filter(fun is_prop_fun/1, Forms),
  io:format("~p~n", [FiltForms]).

% TODO: Fix predicate for filtering proper calls
is_prop_fun({function,_,_,0,_}) -> true;
is_prop_fun(_) -> false.