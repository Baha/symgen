-module(symgen).
-export([main/1]).

main(File) ->
  Forms = forms:read(File ++ ".erl"),
  FiltForms = forms:filter(fun is_prop_fun/1, Forms),
  io:format("~p~n", [FiltForms]).

is_prop_fun({function,_,_,0,Clauses}) ->
  % TODO: Change hd to something else?
  case hd(Clauses) of
    {clause,_,_,_,Body} ->
      case hd(Body) of
        {call,_,{remote,_,{atom,_,proper},{atom,_,forall}},_} -> true;
        _ -> false
      end;
    _ -> false
  end;
is_prop_fun(_) -> false.
