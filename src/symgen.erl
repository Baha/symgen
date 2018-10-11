-module(symgen).
-export([main/1]).

main(File) ->
  Forms = forms:read(File ++ ".erl"),
  io:format("~p~n", [Forms]).
