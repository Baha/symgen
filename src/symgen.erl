-module(symgen).
-export([main/1]).

main(File) ->
  Forms = forms:read(File ++ ".erl"),
  PropFuns = forms:filter(fun is_prop_fun/1, Forms),
  [generate_clauses(Fun) || Fun <- PropFuns].

is_prop_fun({function,_,_,0,FunClauses}) ->
  FirstClause = hd(FunClauses),
  Body = erl_syntax:clause_body(FirstClause),
  case hd(Body) of
    {call,_,{remote,_,{atom,_,proper},{atom,_,forall}},_} -> true;
    {call,_,{remote,_,{atom,_,proper_types},{atom,_,add_constraint}},_} -> true;
    {call,_,{remote,_,{atom,_,proper_types},{atom,_,bind}},_} -> true;
    _ -> false
  end;
is_prop_fun(_) -> false.

generate_clauses(Fun) ->
  {function,_,_,0,Clauses} = Fun,
  {clause,_,_,_,Body} = hd(Clauses),
  {call,_,{remote,_,{atom,_,ProperMod},{atom,_,ProperCall}},CallArgs} = hd(Body),
  Types = lists:nth(1, CallArgs),
  Prop  = lists:nth(2, CallArgs),
  Vars = get_vars(Prop),
  FunName = erl_syntax:function_name(Fun),
  io:format("~s -- ", [forms:from_abstract(FunName)]),
  case {ProperMod, ProperCall} of
    {proper,forall} ->
      io:format("FORALL (property)~n");
    {proper_types, add_constraint} ->
      io:format("SUCHTHAT (user-defined type)~n");
    {proper_types, bind} ->
      io:format("LET (user-defined type)~n")
  end,
  io:format("Vars:  ~s~n", [forms:from_abstract(Vars)]),
  io:format("Types: ~s~n", [forms:from_abstract(Types)]),
  % io:format("PROP:  ~s~n", [forms:from_abstract(Prop)]),
  io:format("~n").

get_vars({'fun',_,{clauses,Clauses}}) ->
  FirstClause = hd(Clauses),
  {clause,_,Vars,_,_} = FirstClause,
  Vars.
  

