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
  PropFun = get_propfun(Prop),
  FunName = erl_syntax:function_name(Fun),
  GenName = forms:from_abstract(FunName),
  case {ProperMod, ProperCall} of
    {proper,forall} ->
      % io:format("FORALL (property)~n"),
      ok;
    {proper_types, add_constraint} ->
      % io:format("SUCHTHAT (user-defined type)~n"),
      ok;
    {proper_types, bind} ->
      % io:format("LET (user-defined type)~n"),
      ok
  end,
  ZipVT = zip_vars_types(Vars, [Types]),
  HeadStr  = pp_head(GenName, ZipVT),
  TypesStr = pp_vars_types(ZipVT),
  PropStr = "eval(" ++ pp_propfun(PropFun) ++ ",Env,Exp)",
  PpStr = HeadStr ++ TypesStr ++ "," ++ PropStr ++ ".",
  % PpStr = HeadStr ++ TypesStr ++ ".",
  io:format("~s~n~n", [PpStr]).%,
  % io:format("~p~n", [PropFun]).

get_vars({'fun',_,{clauses,Clauses}}) ->
  FirstClause = hd(Clauses),
  {clause,_,Vars,_,_} = FirstClause,
  Vars.

get_propfun({'fun',_,{clauses,Clauses}}) ->
  FirstClause = hd(Clauses),
  [Body] = erl_syntax:clause_body(FirstClause),
  Body.

zip_vars_types([], []) -> [];
zip_vars_types([{tuple,_,VList}|RVars], [{tuple,_,TList}|RTypes]) ->
  zip_vars_types(VList,TList) ++ zip_vars_types(RVars,RTypes);
zip_vars_types([V|RVars],[T|RTypes]) ->
  [{V,T}] ++ zip_vars_types(RVars,RTypes).

pp_head(GenName, VarsTypes) ->
  VarsStr = [pp_var(V) || {V,_} <- VarsTypes],
  JointStr = string:join(VarsStr, ","),
  HeadStr = "gen_" ++ GenName ++ "((" ++ JointStr ++ ")) :- ",
  HeadStr.
pp_var(ConsVars = {cons,_,_,_}) ->
  ConsList = forms:cons_to_list(ConsVars),
  PpVars = [pp_var(V) || V <- ConsList],
  string:join(PpVars, ",");
pp_var({tuple,_,Vars}) ->
  PpVars = [pp_var(V) || V <- Vars],
  string:join(PpVars, ",");
pp_var({var,_,Var}) ->
  atom_to_list(Var).

pp_vars_types(VarsTypes) ->
  TypeofStr = [pp_vt(V,T) || {V,T} <- VarsTypes],
  string:join(TypeofStr, ",").

pp_vt({var,_,Var}, {call,_,{atom,_,CName},Args}) ->
  VarStr = atom_to_list(Var),
  CallStr = atom_to_list(CName),
  ArgsStr =
    case length(Args) of
      0 -> "";
      _ -> "(" ++ pp_nested(Args) ++ ")"
    end,
  FullStr = "typeof(" ++ VarStr ++ "," ++ CallStr ++ ArgsStr ++ ")",
  FullStr;
% Args will be empty in this case
pp_vt(Vars,{call,_,{atom,_,CName},_Args}) ->
  CallStr = atom_to_list(CName),
  ArgsStr = "(" ++ pp_var(Vars) ++ ")",
  GenStr = "gen_" ++ CallStr ++ "(" ++ ArgsStr ++ ")",
  GenStr.

pp_nested([{call,_,{atom,_,CName},Args}]) ->
  CallStr = atom_to_list(CName),
  ArgsStr =
      case length(Args) of
      0 -> "";
      _ -> "(" ++ pp_nested(Args) ++ ")"
    end,
  CallStr ++ ArgsStr;
pp_nested(Args) ->
  FromAbsArgs = [forms:from_abstract(A) || A <- Args],
  string:join(FromAbsArgs, ",").

pp_propfun({block,_,List}) ->
  PpList = [pp_propfun(L) || L <- List],
  StrList = "[" ++ string:join(PpList, ",") ++ "]",
  "block(" ++ StrList ++ ")";
pp_propfun({call,_,Call,Args}) ->
  PpArgs = [pp_propfun(A) || A <- Args],
  StrArgs = "[" ++ string:join(PpArgs, ",") ++ "]",
  CallPred =
    case Call of
      {remote,_,_,_} -> "call";
      _ -> "apply"
    end,
  CallPred ++ "(" ++ pp_propfun(Call) ++ "," ++ StrArgs ++ ")";
pp_propfun({op,_,Op,Arg}) ->
  pp_propfun({call,0,{remote,0,{atom,0,erlang},{atom,0,Op}},[Arg]});
pp_propfun({op,_,Op,Arg1,Arg2}) ->
  pp_propfun({call,0,{remote,0,{atom,0,erlang},{atom,0,Op}},[Arg1,Arg2]});
pp_propfun({atom,_,Atom}) ->
  "lit(atom,'" ++ atom_to_list(Atom) ++ "')";
pp_propfun({remote,_,Mod,Fun}) ->
  pp_propfun(Mod) ++ "," ++ pp_propfun(Fun);
pp_propfun({var,_,Var}) ->
  "var('" ++ atom_to_list(Var) ++ "')";
pp_propfun(_) -> "_".
