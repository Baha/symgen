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
  % [FVars] = Vars,
  ZipVT = zip_vars_types(Vars, [Types]),
  PpStr = pp_vars_types(ZipVT),

  % io:format("~n"),
  % io:format("Vars:  ~p~n", [Vars]),
  % io:format("Types: ~p~n", [Types])
  io:format("~s~n", [PpStr]),
  % io:format("~s", [generate_head(ZipVT)]),
  % io:format("~s", [generate_body(ZipVT)]),
  io:format("~n~n").

get_vars({'fun',_,{clauses,Clauses}}) ->
  FirstClause = hd(Clauses),
  {clause,_,Vars,_,_} = FirstClause,
  Vars.

zip_vars_types([], []) -> [];
zip_vars_types([{tuple,_,VList}|RVars], [{tuple,_,TList}|RTypes]) ->
  zip_vars_types(VList,TList) ++ zip_vars_types(RVars,RTypes);
zip_vars_types([V|RVars],[T|RTypes]) ->
  [{V,T}] ++ zip_vars_types(RVars,RTypes).

pp_vars_types(VarsTypes) ->
  TypeofStr = [pp_vt(V,T) || {V,T} <- VarsTypes],
  string:join(TypeofStr, ",").

pp_vt({var,_,Var}, T={call,_,{atom,_,CName},Args}) ->
  VarStr = atom_to_list(Var),
  CallStr = atom_to_list(CName),
  ArgsStr =
    case length(Args) of
      0 -> "";
      _ -> "(" ++ pp_nested(Args) ++ ")"
    end,
  FullStr = "typeof(" ++ VarStr ++ "," ++ CallStr ++ ArgsStr ++ ")",
  FullStr.

pp_nested([{call,_,{atom,_,CName},Args}]) ->
  CallStr = atom_to_list(CName),
  ArgsStr =
      case length(Args) of
      0 -> "";
      _ -> "(" ++ pp_nested(Args) ++ ")"
    end,
  CallStr ++ ArgsStr.

generate_head([Vars]) ->
  "gen(" ++ generate_head_1(Vars) ++ ") :- ".

generate_head_1(ConsList = {cons,_,_,_}) ->
  generate_head_1(forms:cons_to_list(ConsList));

generate_head_1(VList) when is_list(VList) ->
  VarList = [generate_head_1(V) || V <- VList],
  "(" ++ string:join(VarList, ",") ++ ")";

generate_head_1(TupleList = {tuple,_,TupleEs}) ->
  VarList = [generate_head_1(V) || V <- TupleEs],
  "(" ++ string:join(VarList, ",") ++ ")";

generate_head_1(Var={var,_,_}) ->
  forms:from_abstract(Var).
