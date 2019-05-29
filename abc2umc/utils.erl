-module(utils).
-export([build_update/3,    % evalulation of update, no vars
	 build_apred/3,     % evaluation of aware pred
	 build_spred/3,
	 build_rpred/5,
	 build_pc_guard/1,
	 build_pc_list/2,
	 build_outE/3]).

%% build updates
build_update(Assignments,I,B) ->
    ets:new(temp_set, [named_table]),
    build_update(Assignments,I,B,[]).

%% build_update(Assignments,I,V,B) ->
%%     io:format("Build update commands ~p~n with Vars ~p Bound ~p (comp index = ~p) ~n",[Assignments,V,B,I]),
%%     %% Clean dynamic variables created before
%%     build_update(Assignments,I,V,B,[]).

build_update([],_,_,S) ->
    ets:delete(temp_set),
    lists:reverse(S);
build_update([H|T], I, B, S) ->
%    io:format("Build update for ~p~n with Vars ~p Bound ~p args = ~p) ~n",[H,V,B,A]),
    build_update(T,I,B,[evalu(H,I,B)|S]).

build_apred(Pred, I, B) ->
%   io:format("build aware pred ~p with i ~p, b ~p ~n",[Pred,I,B]),
    List = "("++evala(Pred, I, B)++")".

%% this functions returns code for subpredicates if there any (to deal with membership predicate) and code for the input predicate Pred
build_spred(Pred, I, B) ->
   % io:format("build sending pred ~p ~n",[Pred]),
    %% create temporary names for variables
    ets:new(temp_name, [named_table]),
%    ets:insert(temp_name, {c,0}),
    Ret2 = "(" ++ evals(Pred, I, B) ++ ")", % return either a real predicate or a boolean variable indicating membership
    ets:match_delete(temp_name, {c, '_'}),
    Ret1 = string:join([X || {_,X} <- ets:tab2list(temp_name)], "\n"),
  %  io:format("Going to return Subpred, Ret ~p ~p~n",[Ret1, Ret2]),
    ets:delete(temp_name),
    [Ret1, Ret2].

%% receiving predicates can refer to previous variables in Bound or new variables in the message
build_rpred(Pred, I,  B, M, {}) ->
    "(" ++ evalr(Pred, I,  B, M) ++ ")";
build_rpred(Pred, _,  B, M, {_,I,_}) ->
    "(" ++ evalr(Pred, I,  B, M) ++ ")".

evalu({self,Att}, _, _) ->
    Att ++ "[$1]";
evalu({att,Att}, _,_) ->
    Att ++ "[$1]";
evalu({parenthesis,P},I, B) ->
    "(" ++ evalu(P,I, B) ++ ")";
evalu({bracket,E}, I, B) ->
    "[" ++ evalu(E,I, B) ++ "]";
evalu({bracket2,E}, I, B) ->  % set as vector
    "[" ++ evalu(E,I, B) ++ "]";
evalu({head,Name}, I, B) -> evalu(Name,I,B) ++ ".head";
evalu({tail,Name}, I, B) -> evalu(Name,I,B) ++ ".tail";
evalu({min,[Name]}, I, B) ->
    ets:insert(temp_set,{op,".min"}),
    evalu(Name,I,B);
evalu({max,[Name]}, I, B) ->
    ets:insert(temp_set,{op,".max"}),
    evalu(Name,I,B);
evalu({min,Name}, I, B) -> evalu(Name,I,B) ++ ".min";
evalu({max,Name}, I, B) -> evalu(Name,I,B) ++ ".max";
evalu({selector,List,Index}, I,B) -> evalu(List,I,B) ++ "[" ++ evalu(Index,I,B) ++ "]";
evalu({length, R}, I, B) ->
    evalu(R, I, B) ++ ".length";
evalu("true", _,_) -> "true";
evalu("false", _,_) -> "false";
evalu(empty_vector, _,_) -> "[]";
evalu(empty_set, _,_) -> "[]";
evalu({const,C}, _,_) -> C;
evalu({minusconst,C}, _,_) -> "-" ++ C;
evalu([H|T] = L,I,B) -> %really bracket of a list
    string:join([evalu(X,I, B) || X <- L],",");
evalu({'+',L,R}, I, B) -> evalu(L,I,B) ++ "+" ++ evalu(R,I,B);
evalu({'-',L,R}, I, B) -> evalu(L,I,B) ++ "-" ++ evalu(R,I,B);
evalu({'*',L,R}, I, B) -> evalu(L,I,B) ++ "*" ++ evalu(R,I,B);
evalu({'++',L,R}, I, B) ->  % ordered set operation
    LHS = evalu(L,I,B),
    RHS = evalu(R,I,B),
    ets:insert(temp_set,{set,true}),
    LHS ++ "+" ++ RHS;
evalu({'--',L,R}, I, B) ->  % ordered set operation
    LHS = evalu(L,I,B),
    RHS = evalu(R,I,B),
    ets:insert(temp_set,{set,true}),
    LHS ++ "-" ++ RHS;
evalu([], _,_) -> [];
evalu({param, T}, I, B) ->
    %io:format("information before build ~p ~p ~p~n",[T,I,B]),
    "bound[$1][" ++ integer_to_list(I) ++ "][" ++ integer_to_list(proplists:get_value(T,B)) ++ "]";
evalu({token,T}, _,_) ->
    L = get(token),
    put(token, L#{T => T}),
    T;
evalu({concat,L,R}, I, B) ->
    evalu(L,I,B) ++ "+"  ++ evalu(R,I,B);
evalu({assign, L, R}, I,B) ->
    % remeber if this is a set
    ets:insert(temp_set, {set, false}),
    ets:insert(temp_set, {op, ""}),
    Right =  evalu(R, I, B),
    S = ets:lookup_element(temp_set, set, 2),
    OP = ets:lookup_element(temp_set, op, 2),
    if not S ->
	    ets:insert(temp_set, {op,""}),
	    evalu(L, I, B) ++ " := " ++ Right;
       true ->
	    ets:insert(temp_set, {set,false}),
	    ets:insert(temp_set, {op,""}),
	    "vectemp: obj := " ++ Right ++ ";\n\t\t" ++ evalu(L, I, B) ++ ":= vectemp.toset" ++ OP
    end;
evalu({eq, L, R}, I,B) ->
    evalu(L, I, B) ++ " = " ++ evalu(R, I, B);
evalu({diff, L, R}, I, B) ->
    evalu(L, I, B) ++ " != " ++ evalu(R, I, B);
evalu({ge, L, R}, I, B) ->
    evalu(L, I, B) ++ " > " ++ evalu(R, I, B);
evalu({geq, L, R}, I, B) ->
    evalu(L, I, B) ++ " >= " ++ evalu(R, I, B);
evalu({le, L, R}, I, B) ->
    evalu(L, I, B) ++ " < " ++ evalu(R, I, B);
evalu({intersect, L, R}, I, B) ->
    evalu(L, I, B) ++ " and " ++ evalu(R, I,B);
evalu({union, L, R}, I, B) ->
    evalu(L, I, B) ++ " or " ++ evalu(R, I, B);
evalu({negation, R}, I, B) ->
    " not " ++ evalu(R, I, B);
evalu({ismember, L, R}, I, B) ->
    L1 = evalu(L, I, B),
%    io:format("lef side ~p~n",[L]),
    R1 = evalu(R, I, B),
    Temp = [L1 ++ "=" ++ R1 ++"["++integer_to_list(Int)++"]" || Int <- lists:seq(0,7)],
    Member = "(false or " ++ string:join(Temp, " or ") ++ ")";
evalu({notmember, L, R}, I, B) ->
    "(not" ++ evalu({ismember, L, R}, I, B) ++ ")";
evalu({var,Name}, _, B) ->
    I1 = proplists:get_value(Name,B),
    if is_integer(I1) -> "msg["++integer_to_list(I1)++"]";
       true  -> I1
    end.

evala({self,Att}, _, _) ->
    Att ++ "[$1]";
evala({att,Att}, _,_) ->
    Att ++ "[$1]";
evala({parenthesis,P},I, B) ->
    "(" ++ evala(P,I, B) ++ ")";
evala({bracket,E}, I, B) ->
    "[" ++ evala(E,I, B) ++ "]";
evala({bracket2,E}, I, B) ->  %set as vector
    "[" ++ evala(E,I, B) ++ "]";
evala({head,Name}, I, B) -> evala(Name,I,B) ++ ".head";
evala({tail,Name}, I, B) -> evala(Name,I,B) ++ ".tail";
evala({selector,List,Index}, I,B) -> evala(List,I,B) ++ "[" ++ evala(Index,I,B) ++ "]";
evala({length, R}, I, B) ->
    evala(R, I, B) ++ ".length";
evala("true", _,_) -> "true";
evala("false", _,_) -> "false";
evala(empty_vector, _,_) -> "[]";
evala(empty_set, _,_) -> "[]"; %%currently use vector for set
evala({const,C}, _,_) -> C;
evala({minusconst,C}, _,_) -> "-" ++ C;
evala({'++',L,R}, I, B) ->
    evala(L,I,B) ++ "+" ++ evala(R,I,B);
evala({'+',L,R}, I, B) ->  % ordered set operation
    evala(L,I,B) ++ "+" ++ evala(R,I,B);
evala({'--',L,R}, I, B) ->
    evala(L,I,B) ++ "-" ++ evala(R,I,B);
evala({'-',L,R}, I, B) ->
    evala(L,I,B) ++ "-" ++ evala(R,I,B);
evala([], _,_) -> "[]";
evala({param, T}, I, B) ->
    %io:format("information before build ~p ~p ~p~n",[T,I,B]),
    "bound[$1][" ++ integer_to_list(I) ++ "][" ++ integer_to_list(proplists:get_value(T,B)) ++ "]";
evala({token,T}, _,_) ->
    L = get(token),
    put(token, L#{T => T}),
    T;
evala({eq, L, R}, I,B) ->
    evala(L, I, B) ++ " = " ++ evala(R, I, B);
evala({diff, L, R}, I, B) ->
    evala(L, I, B) ++ " != " ++ evala(R, I, B);
evala({ge, L, R}, I, B) ->
    evala(L, I, B) ++ " > " ++ evala(R, I, B);
evala({geq, L, R}, I, B) ->
    evala(L, I, B) ++ " >= " ++ evala(R, I, B);
evala({le, L, R}, I, B) ->
    evala(L, I, B) ++ " < " ++ evala(R, I, B);
evala({leq, L, R}, I, B) ->
    evala(L, I, B) ++ " <= " ++ evala(R, I, B);
evala({intersect, L, R}, I, B) ->
    evala(L, I, B) ++ " and " ++ evala(R, I,B);
evala({union, L, R}, I, B) ->
    evala(L, I, B) ++ " or " ++ evala(R, I, B);
evala({negation, R}, I, B) ->
    " not " ++ evala(R, I, B);
evala({ismember, L, R}, I, B) ->
    L1 = evala(L, I, B),
%    io:format("lef side ~p~n",[L]),
    R1 = evala(R, I, B),
    L1 ++ " in " ++ R1;
    %% Temp = [L1 ++ "=" ++R1++"["++integer_to_list(Int)++"]" || Int <- lists:seq(0,7)],
    %% Member = "(false or " ++ string:join(Temp, " or ") ++ ")";
evala({notmember, L, R}, I, B) ->
    "not " ++ evala({ismember, L, R}, I, B);
evala({var,Name}, _, B) ->
    I1 = proplists:get_value(Name,B),
    if is_integer(I1) -> "msg["++integer_to_list(I1)++"]";
       true  -> I1
    end.

evala({parenthesis,P},I, V,B) ->
    "(" ++ evala(P,I, V,B) ++ ")";
evala({bracket,E}, I, V,B) ->
    "[" ++ evala(E,I, V,B) ++ "]";
evala({bracket2,E}, I, V,B) ->  %set as vector
    "[" ++ evala(E,I, V,B) ++ "]";
evala({head,Name}, I, V,B) -> evala(Name,I,V,B) ++ ".head";
evala({tail,Name}, I, V,B) -> evala(Name,I,V,B) ++ ".tail";
evala({selector,List,Index}, I,V,B) -> evala(List,I,V,B) ++ "[" ++ evala(Index,I,V,B) ++ "]";
evala("true", _,_,_) -> "true";
evala("false", _,_,_) -> "false";
evala({self,Att}, I, V,B) ->
    Att ++ "[$1]";
evala({att,Att},_, _,_) -> Att ++ "[j]";
evala({const,C},_, _,_) -> C;
evala({minusconst,C}, _,_,_) -> "-" ++ C;
evala({'+',L,R}, I, V, B) -> evala(L,I,V,B) ++ "+" ++ evala(R,I,V,B);
evala({'*',L,R}, I, V, B) -> evala(L,I,V,B) ++ "*" ++ evala(R,I,V,B);
evala([],_,_,_) -> "[]";
evala({param,T}, I,_, B) ->
    "bound[$1]" ++ "[" ++ integer_to_list(I) ++ "][" ++ integer_to_list(proplists:get_value(T,B)) ++ "]";
evala({token,T}, _,_,_) ->
    L = get(token),
    put(token, L#{T => T}),
    T;
evala({concat,L,R}, I, V,B) ->
    evala(L,I,V,B) ++ "+"  ++ evala(R,I,V,B);
evala({eq, L, R}, I, V,B) ->
    evala(L, I, V,B) ++ " = " ++ evala(R, I, V,B);
evala({diff, L, R}, I, V,B) ->
    evala(L, I, V,B) ++ " != " ++ evala(R, I, V,B);
evala({ge, L, R}, I, V,B) ->
    evala(L, I, V,B) ++ " > " ++ evala(R, I, V,B);
evala({geq, L, R}, I, V,B) ->
    evala(L, I, V,B) ++ " >= " ++ evala(R, I, V,B);
evala({le, L, R}, I, V,B) ->
    evala(L, I, V,B) ++ " < " ++ evala(R, I, V,B);
evala({intersect, L, R}, I, V,B) ->
    evala(L, I, V,B) ++ " and " ++ evala(R, I, V,B);
evala({union, L, R}, I, V,B) ->
    evala(L, I, V,B) ++ " or " ++ evala(R, I, V,B);
evala({negation, R}, I, V,B) ->
    " not " ++ evala(R, I, V,B);
evala({var,Name}, _, V,B) ->
    I1 = proplists:get_value(Name,V,B),
    if is_integer(I1) -> "msg["++integer_to_list(I1)++"]";
       true  -> I1
    end;
evala({ismember, L, R}, I, V,B) ->
    L1 = evala(L, I, V,B),
    R1 = evala(R, I, V,B),
    Temp = [L1 ++ "=" ++R1++"["++integer_to_list(Int)++"]" || Int <- lists:seq(0,7)],
    Member = "(" ++ string:join(Temp, " and ") ++ ")".


build_pc_guard({[],{Pcname, Pinstance, Value}}) ->
    Pcname ++ "[" ++ integer_to_list(Pinstance) ++ "] = " ++ integer_to_list(Value);

build_pc_guard({Parent, {Pcname, P2, V2}}) ->
    build_parent(Parent) ++ Pcname ++ "[" ++ integer_to_list(P2) ++ "] = " ++ integer_to_list(V2).

% actually there is only one parent length(L) = 1
build_parent({}) ->
    "";
build_parent({Pcname,P,V}) ->
    Pcname ++ "[" ++ integer_to_list(P) ++ "] = " ++ integer_to_list(V) ++ " and ".


build_pc_list(L) ->
    build_pc_list(L,[]).

build_pc_list([],L) ->
    "[" ++ string:join(L,",") ++ "]";
build_pc_list([H|T],L) ->
    print("NUM PROCS: "),
    print(H),
    H1 = lists:seq(1,H),
    S = ["1" || _ <- H1],
    String = "[" ++ string:join(S, ",") ++ "]",
    build_pc_list(T,[String | L]).

build_choice_list(N) ->
    L = lists:seq(1,N),
    S = ["[1]" || _ <- L],
    "[" ++ string:join(S,",") ++ "]".


%% evaluation of eval send
evals({parenthesis,P}, I, B) ->
    "(" ++ evals(P, I, B) ++ ")";
evals({bracket,E}, I, B) ->
    "[" ++ evals(E,I, B) ++ "]";
evals({bracket2,E}, I, B) -> %% sets as vector
    "[" ++ evals(E,I, B) ++ "]";
evals({head,N}, I, B) -> evals(N,I,B) ++ ".head";
evals({tail,N}, I, B) -> evals(N,I,B) ++ ".tail";

%% NEED TO REVIEW
evals({selector,List,Index}, I,B) -> evals(List,I,B) ++ "[" ++ evals(Index,I,B) ++ "]";
evals("true",_, _) -> "true";
evals("false",_, _) -> "false";
evals({var,Att},_, _) -> Att ++ "[j]";  %% inside sending predicaets, there is no variables (carried by msg as in case of receiving predicate.. this appear here because the parser cannot distinguish if a term is a variable or an attribtue of a different component.
evals({att,Att}, _, _) -> Att ++ "[j]";
evals({param,T}, I, B) ->
    "bound[$1][" ++ integer_to_list(I) ++ "]" ++ "[" ++ integer_to_list(proplists:get_value(T,B)) ++"]";
evals({self,Att}, _,_) ->
    Att ++ "[$1]";
evals({'+',L,R}, I, B) ->
    evals(L,I,B) ++ "+" ++ evals(R,I,B);
evals({'*',L,R}, I, B) ->
    evals(L,I,B) ++ "*" ++ evals(R,I,B);

evals({const,C}, _,_) -> C;
evals({minusconst,C},_,_) -> "-" ++ C;
evals([],_, _) -> "[]";
evals({token,T},_, _) ->
    L = get(token),
    put(token, L#{T => T}),
    T;
evals([{self,Att}=Term], I,B) ->
    "[" ++ evals(Term, I,B) ++ "]";
evals([{var,Att}=Term], I,B) ->
    "[" ++ evals(Term, I,B) ++ "]";

evals([H|T]=List,I,B) when T =/= [] ->
    S = [evals(Name,I,B) || Name <- List],
    "[" ++ string:join(S,",") ++ "]";

evals({eq, L, R}, I,B) ->
    evals(L, I,B) ++ " = " ++ evals(R, I,B);
evals({diff, L, R}, I,B) ->
    evals(L, I,B) ++ " != " ++ evals(R, I,B);
evals({ge, L, R}, I,B) ->
    evals(L, I,B) ++ " > " ++ evals(R, I,B);
evals({geq, L, R}, I,B) ->
    evals(L, I,B) ++ " >= " ++ evals(R, I,B);
evals({leq, L, R}, I,B) ->
    evals(L, I,B) ++ " <= " ++ evals(R, I,B);
evals({le, L, R}, I,B) ->
    evals(L, I,B) ++ " < " ++ evals(R, I,B);
evals({intersect, L, R}, I,B) ->
    evals(L, I,B) ++ " and " ++ evals(R, I,B);
evals({union, L, R}, I,B) ->
    evals(L, I,B) ++ " or " ++ evals(R, I,B);
evals({notmember, L, R}, I,B) ->
    %% L1= evals(L, I,B),
    %% R1 = evals(R, I,B),
    %% C = "tumccounter" ++ integer_to_list(ets:update_counter(temp_name, c, 1, {c, 0})),
    %% SubPred = build_subpred(L1,R1,C,"="),
    %% ets:insert(temp_name, {C, SubPred}),
    "not " ++ evals({ismember, L, R}, I,B);
evals({ismember, L, R}, I,B) ->
    L1= evals(L, I,B),
    R1 = evals(R, I,B),
    L1 ++ " in " ++ R1;
evals({negation, T}, I,B) ->
    " not " ++ evals(T,I,B).

%% evaluation of eval receive predicate
evalr({parenthesis,P}, I, B, M) ->
    "(" ++ evalr(P, I, B, M) ++ ")";
evalr({bracket,E}, I, B, M) ->
    "[" ++ evalr(E,I,B, M) ++ "]";
evalr({bracket2,E}, I, B, M) ->  %set as vector for now
    "[" ++ evalr(E,I,B, M) ++ "]";
evalr({head,N}, I, B, M) -> evalr(N,I,B,M) ++ ".head";
evalr({tail,N}, I, B, M) -> evalr(N,I,B,M) ++ ".tail";

%% NEED TO REVIEW
evalr({selector,List,Index}, I,B,M) -> evalr(List,I,B,M) ++ "[" ++ evalr(Index,I,B,M) ++ "]";
evalr("true", _,_,_) -> "true";
evalr("false",_, _,_) -> "false";
evalr({att,Att}, _, _,_) -> Att ++ "[j]";
evalr({param,T},I,B,_) ->
    "bound[$1]" ++ "[" ++ integer_to_list(I) ++ "][" ++ integer_to_list(proplists:get_value(T,B)) ++ "]";
evalr({self,Att}, I,_,M) ->
    Att ++ "[$1]";
evalr({'+',L,R}, I,B,M) ->
    evalr(L,I,B,M) ++ "+" ++ evalr(R,I,B,M);
evalr({'-',L,R}, I,B,M) ->
    evalr(L,I,B,M) ++ "-" ++ evalr(R,I,B,M);
evalr({'*',L,R}, I,B,M) ->
    evalr(L,I,B,M) ++ "*" ++ evalr(R,I,B,M);

evalr({const,C}, _,_,_) -> C;
evalr({minusconst,C}, _,_,_) -> "-" ++ C;
evalr([], _,_,_) -> "[]";
evalr({token,T}, _,_,_) ->
    L = get(token),
    put(token, L#{T => T}),
    T;
evalr([{self,Att}=Term], I,B,M) ->
    "[" ++ evalr(Term, I,B,M) ++ "]";

evalr([H|T]=List,I,B,M) when T =/= [] ->
    S = [evalr(Name,I,B,M) || Name <- List],
    "[" ++ string:join(S,",") ++ "]";

evalr({eq, L, R}, I,B,M) ->
    evalr(L, I,B,M) ++ " = " ++ evalr(R, I,B,M);
evalr({diff, L, R}, I,B,M) ->
    evalr(L, I,B,M) ++ " != " ++ evalr(R, I,B,M);
evalr({ge, L, R}, I,B,M) ->
    evalr(L, I,B,M) ++ " > " ++ evalr(R, I,B,M);
evalr({geq, L, R}, I,B,M) ->
    evalr(L, I,B,M) ++ " >=" ++ evalr(R, I,B,M);
evalr({le, L, R}, I,B,M) ->
    evalr(L, I,B,M) ++ " < " ++ evalr(R, I,B,M);
evalr({leq, L, R}, I,B,M) ->
    evalr(L, I,B,M) ++ " <= " ++ evalr(R, I,B,M);
evalr({intersect, L, R}, I,B,M) ->
    evalr(L, I,B,M) ++ " and " ++ evalr(R, I,B,M);
evalr({union, L, R}, I,B,M) ->
    evalr(L, I, B,M) ++ " or " ++ evalr(R, I,B,M);
evalr({notmember, L, R}, I,B,M) ->
    L1= evalr(L, I,B,M),
    R1 = evalr(R, I,B,M),
    Temp = [L1 ++ "!=" ++ R1 ++"["++integer_to_list(Int)++"]" || Int <- lists:seq(0,7)],
    Member = "(" ++ string:join(Temp, " and ") ++ ")";
evalr({ismember, L, R}, I,B,M) ->
    L1= evalr(L, I,B,M),
    R1 = evalr(R, I,B,M),
    Temp = [L1 ++ "=" ++ R1 ++"["++integer_to_list(Int)++"]" || Int <- lists:seq(0,7)],
    Member = "(" ++ string:join(Temp, " and ") ++ ")";
evalr({negation, T}, I, B,M) ->
    " not " ++ evalr(T,I,B,M);
evalr({var,Name}, _, _, M) ->
    I = proplists:get_value(Name,M),
%    io:format("M = ~p, Name = ~p ~n", [M,Name]),
    "msg["++integer_to_list(I)++"]".


build_outE(Exps,I, B) ->
    build_outE(Exps, I, B, []).

build_outE([],_,_, S) -> %
    S2 = lists:map(fun({I,{E1,E}}) ->
			   E1 ++ "msg" ++ integer_to_list(I) ++ ":obj := " ++ E;
		      ({I,E}) ->
			   "msg" ++ integer_to_list(I) ++ ":obj := " ++ E
		   end,
			lists:zip(lists:seq(1,length(S)),lists:reverse(S))),
    Elem = string:join(S2,";\n\t\t") ++ ";",
    Msg = "[" ++
	string:join(lists:map(fun(I) ->
				      "msg" ++ integer_to_list(I)
			      end,
			      lists:seq(1,length(S))),",")
	++ "]",
    [Elem,Msg];
build_outE([H|T], I, B, S) ->
    build_outE(T,I,B,[evale(H,I,B)|S]).

%% UMC does not support sending compound expressions
%% Thus need to represent them as temporal variables

evale([E], I,B) -> % E is wrapped by a parenthesis
    "(" ++ evale(E, I,B) ++ ")";
evale({'+',L,R}, I,B) -> evale(L,I,B) ++ "+" ++ evale(R,I,B);
evale({'*',L,R}, I,B) -> evale(L,I,B) ++ "*" ++ evale(R,I,B);
evale({'-',L,R}, I,B) -> evale(L,I,B) ++ "-" ++ evale(R,I,B);
evale({'--',L,R}, I,B) -> evale(L,I,B) ++ "-" ++ evale(R,I,B);
evale({'++',L,R}, I,B) -> evale(L,I,B) ++ "+" ++ evale(R,I,B);
evale({head,Name}, I,B) ->
    evale(Name,I,B) ++ ".head";
evale({tail,Name}, I,B) ->
    evale(Name,I,B) ++ ".tail";
evale({min,[Name]}, I,B) ->  % umc does not support (E).op yet !!
    {"tmp: obj := " ++ evale(Name,I,B) ++ ";\n\t\t", "tmp.min"};
evale({min,Name}, I,B) ->
    evale(Name,I,B) ++ ".min";
evale({max,[Name]}, I,B) ->  % umc does not support (E).op yet !!
    {"tmp: obj := " ++ evale(Name,I,B) ++ ";\n\t\t", "tmp.max"};
evale({max,Name}, I,B) ->
    evale(Name,I,B) ++ ".max";

evale({selector,List,Index}, I,B) -> evale(List,I,B) ++ "[" ++ evale(Index,I,B) ++ "]";
evale({self,Att}, I,B) -> Att ++ "[$1]";
evale({att,Att}, I,B) -> evale({self,Att}, I,B);
evale({param,T}, I,B) ->
        "bound[$1]" ++ "[" ++ integer_to_list(I) ++ "][" ++ integer_to_list(proplists:get_value(T,B)) ++ "]";
evale({const,C}, _,_) -> C;
evale({minusconst,C},_, _) -> "-" ++ C;
evale({token,T}, _,_) ->
    L = get(token),
    put(token, L#{T => T}),
    T.


remove_dups([])    -> [];
remove_dups([{Fi,Se,Th,_}=H|T]) -> [H | [X || {Fi1,Se1,Th1,_}=X <- remove_dups(T), {Fi1,Se1,Th1} /= {Fi,Se,Th}]].

print(X) -> io:format("~p~n", [X]).
