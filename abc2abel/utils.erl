-module(utils).
-export([build_update/3,    % evalulation of update, no vars
	 build_apred/2,     % evaluation of aware pred
	 build_spred/2,
	 build_rpred/3,
	 build_msg/1,
	 build_args/2]).

%% build_args of process call
%% A1 - process param
%% A2 - variables bound to messages
build_args(A1,A2) ->
    Index1 = lists:seq(1, length(A2)),
    Msg = lists:zip(A2, Index1),
    evalm(A1,Msg,[]).

evalm([],A2, Acc) ->
    string:join(lists:reverse(Acc),",");
evalm([H|T],A2,Acc) ->
    evalm(T,A2,[evalm(H,A2)|Acc]).

evalm({'+',L,R},A2) -> evalm(L,A2) ++ " + " ++ evalm(R,A2);
evalm({'-',L,R},A2) -> evalm(L,A2) ++ " - " ++ evalm(R,A2);
evalm({'*',L,R},A2) -> evalm(L,A2) ++ " * " ++ evalm(R,A2);
evalm({literal,X},A2) ->
    io:format("what is A2 ~p~n",[A2]),
    case proplists:get_value(X,A2) of
	undefined ->
	    "var(" ++ lists:flatten(io_lib:format("~p",[list_to_atom(X)])) ++ ",V)";
	    %"_" ++ string:uppercase(X);
	I ->
	    "msg(" ++ integer_to_list(I) ++ ",_M)"
    end;
evalm({const,X},_) ->
    X;
evalm({func,Name,L},A2) ->
    "user_code:" ++ Name ++ "(" ++ string:join([evalm(X,A2) || X <- L],",") ++ ")".




%% build updates
build_update(MyAtts,Upd, Msg) ->
    build_update(MyAtts,Upd,Msg,[]).

build_update(_,[],_,S) -> "[" ++ string:join(lists:reverse(S),",\n\t") ++ "]";
build_update(MyAtts,[H|T],Msg,S) ->
    build_update(MyAtts,T,Msg,[evall(MyAtts,H,Msg)|S]).

build_apred(MyAtts,[]) ->
   "fun(_LclE) -> true end";
build_apred(MyAtts,Pred) ->
%   io:format("build aware pred ~p with b ~p ~n",[Pred,B]),
   "fun(_LclE) -> " ++ evall(MyAtts,Pred,[]) ++ " end".



%% this functions returns code for subpredicates if there any (to deal with membership predicate) and code for the input predicate Pred
build_spred(_,[]) ->
   % io:format("build sending pred ~p ~n",[Pred]),
    %% create temporary names for variables
    "fun(_LclE, _RmtE) -> true end";
build_spred(OtherAtts,Pred) ->
   % io:format("build sending pred ~p ~n",[Pred]),
    %% create temporary names for variables
    "fun(_LclE, _RmtE) -> " ++ evals(OtherAtts,Pred) ++ " end".

%% receiving predicates can refer to previous variables in Bound or new variables in the message
build_rpred(_,[], _) ->
    "fun(_LclE, _M, _RmtE) -> true end";
build_rpred(OtherAtts, Pred, M) ->
%    "fun(_LclE, _M, _RmtE) -> msg_size(_M) == " ++ integer_to_list(length(M)) ++ " andalso " ++ evalr(OtherAtts, Pred, M) ++ " end".
    "fun(_LclE, _M, _RmtE) -> " ++ evalr(OtherAtts, Pred, M) ++ " end".


evall(_,{self,Att},_M) ->
    "att(" ++ Att ++ ",_LclE)";
evall(_,{param,Att},_M) ->
    "var(" ++ Att ++ ",_Var)";
evall(Atts,{parenthesis,P},_M) ->
    "(" ++ evall(Atts,P,_M) ++ ")";
evall(Atts,{bracket,L},_M) ->
    "[" ++ string:join([evall(Atts,E,_M) || E <- L], ",") ++ "]";
evall(Atts,{bracket2,L},_M) ->
    "sets:from_list([" ++ string:join([evall(Atts,E,_M) || E <- L], ",") ++ "])";
evall(Atts,{head,Name},_M) -> "hd(" ++ evall(Atts,Name,_M) ++ ")";
evall(Atts,{tail,Name},_M) -> "tl(" ++ evall(Atts,Name,_M) ++ ")";
evall(Atts,{length, R},_M) ->
    "sets:size(" ++ evall(Atts,R,_M) ++ ")";
evall(_,"true",_M) -> "true";
evall(_,"false",_M) -> "false";
evall(_,empty_vector,_M) -> "[]";
evall(_,empty_set,_M) -> "sets:new()";
evall(_,{const,C},_M) -> C;
evall(_,{minusconst,C},_M) -> "-" ++ C;
evall(_,{token,T},_M) ->
    lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evall(Atts,[H|T] = L,_M) -> %really bracket of a list
    string:join([evall(Atts,X,_M) || X <- L],",");
evall(Atts,{'+',L,R},_M) -> evall(Atts,L,_M) ++ " + " ++ evall(Atts,R,_M);
evall(Atts,{'-',L,R},_M) -> evall(Atts,L,_M) ++ " - " ++ evall(Atts,R,_M);
evall(Atts,{'*',L,R},_M) -> evall(Atts,L,_M) ++ " * " ++ evall(Atts,R,_M);
evall(Atts,{concat,L,R},_M) ->
    evall(Atts,L,_M) ++ " + "  ++ evall(Atts,R,_M);
evall(Atts,{assign, {literal, L}, R},_M) when _M =/= []->
    "{" ++ L ++ ", fun(_LclE,_M) -> " ++ evall(Atts,R,_M) ++ " end}";
evall(Atts,{assign, {literal, L}, R},_M) when _M == []->
    "{" ++ L ++ ", fun(_LclE) -> " ++ evall(Atts,R,_M) ++ " end}";
%% evall(Atts,{assign, {self, L}, R},_M) when ->
%%      "{" ++ L ++ ", fun(_LclE) -> " ++ evall(Atts,R,_M) ++ " end}";
evall(Atts,{func,Name,L},_M) -> "user_code:" ++ Name ++ "(" ++ string:join([evall(Atts,C,_M) || C <- L],",") ++ ")";
evall(Atts,{'++',L,R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "sets:union(" ++ L1 ++ "," ++ R1 ++ ")";
evall(Atts,{'--',L,R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "sets:substract(" ++ L1 ++ "," ++ R1 ++ ")";
evall(Atts,[],_M) -> "[]";
evall(Atts,{eq, L, R},_M) ->
    evall(Atts,L,_M) ++ " == " ++ evall(Atts,R,_M);
evall(Atts,{diff, L, R},_M) ->
    evall(Atts,L,_M) ++ " =/= " ++ evall(Atts,R,_M);
evall(Atts,{ge, L, R},_M) ->
    evall(Atts,L,_M) ++ " > " ++ evall(Atts,R,_M);
evall(Atts,{geq, L, R},_M) ->
    evall(Atts,L,_M) ++ " >= " ++ evall(Atts,R,_M);
evall(Atts,{le, L, R},_M) ->
    evall(Atts,L,_M) ++ " < " ++ evall(Atts,R,_M);
evall(Atts,{leq, L, R},_M) ->
    evall(Atts,L,_M) ++ " =< " ++ evall(Atts,R,_M);
evall(Atts,{intersect, L, R},_M) ->
    evall(Atts,L,_M) ++ " andalso " ++ evall(Atts,R,_M);
evall(Atts,{union, L, R},_M) ->
    evall(Atts,L,_M) ++ " orelse " ++ evall(Atts,R,_M);
evall(Atts,{negation, R},_M) ->
    " not " ++ evall(Atts,R,_M);
evall(Atts,{ismember, L, R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "sets:is_element("++L1++","++R1++")";
evall(Atts,{notmember, L, R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "not " ++ "sets:is_element("++L1++","++R1++")";
evall(Atts,{literal, Name},_M) ->
    case lists:member(Name,Atts) of
	true ->
	    "att(" ++ lists:flatten(io_lib:format("~p",[list_to_atom(Name)])) ++ ",_LclE)";
	false ->
	    case proplists:get_value(Name,_M) of
		undefined ->
		    "var("++ lists:flatten(io_lib:format("~p",[list_to_atom(Name)])) ++ ",V)";
		    %"_" ++ string:uppercase(Name);
		I ->
		    "msg(" ++ integer_to_list(I) ++ ",_M)"
	    end
    end.


%% evaluation of eval send
evals(OtherAtts,{parenthesis,P}) ->
    "(" ++ evals(OtherAtts,P) ++ ")";
evals(OtherAtts,{bracket,L}) ->
    "[" ++ string:join([evals(OtherAtts,E) || E <- L], ",") ++ "]";
evals(OtherAtts,{bracket2,L}) ->
    "sets:from_list([" ++ string:join([evals(OtherAtts,E) || E <- L], ",") ++ "])";
evals(OtherAtts,{head,N}) -> evals(OtherAtts,N) ++ ".head";
evals(OtherAtts,{tail,N}) -> evals(OtherAtts,N) ++ ".tail";

%% NEED TO REVIEW
evals(_,"true") -> "true";
evals(_,"false") -> "false";
%% inside sending predicates, there is no variables (carried by msg as in case of receiving predicate.. this appear here because the parser cannot distinguish if a term is a variable or an attribtue of a different component.
evals(OtherAtts,{literal,Name}) ->
    case lists:member(Name,OtherAtts) of
	true ->
	        "att(" ++ Name ++ ",_RmtE)";
	false ->
	    "var("++ lists:flatten(io_lib:format("~p",[list_to_atom(Name)])) ++ ",V)"
%	    "_" ++ string:uppercase(Name)
    end;
evals(OtherAtts,{self,Att}) ->
    "att(" ++ Att ++ ",_LclE)";
evals(_,{param,Att}) ->
    "var(" ++ Att ++ ",_Var)";
evals(OtherAtts,{'+',L,R}) ->
    evals(OtherAtts,L) ++ " + " ++ evals(OtherAtts,R);
evals(OtherAtts,{'*',L,R}) ->
    evals(OtherAtts,L) ++ " * " ++ evals(OtherAtts,R);
evals(OtherAtts,{const,C}) -> C;
evals(OtherAtts,{minusconst,C}) -> "-" ++ C;
evals(OtherAtts,{token,T}) ->
    lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evals(OtherAtts,empty) -> "false";
evals(OtherAtts,[]) -> "[]";
evals(OtherAtts,[{self,Att}=Term]) ->
    "[" ++ evals(OtherAtts,Term) ++ "]";
evals(OtherAtts,[{var,Att}=Term]) ->
    "[" ++ evals(OtherAtts,Term) ++ "]";
evals(OtherAtts,[H|T]=List) when T =/= [] ->
    S = [evals(OtherAtts,Name) || Name <- List],
    "[" ++ string:join(S,",") ++ "]";
evals(OtherAtts,{eq, L, R}) ->
    evals(OtherAtts,L) ++ " == " ++ evals(OtherAtts,R);
evals(OtherAtts,{diff, L, R}) ->
    evals(OtherAtts,L) ++ " =/= " ++ evals(OtherAtts,R);
evals(OtherAtts,{ge, L, R}) ->
    evals(OtherAtts,L) ++ " > " ++ evals(OtherAtts,R);
evals(OtherAtts,{geq, L, R}) ->
    evals(OtherAtts,L) ++ " >= " ++ evals(OtherAtts,R);
evals(OtherAtts,{leq, L, R}) ->
    evals(OtherAtts,L) ++ " =< " ++ evals(OtherAtts,R);
evals(OtherAtts,{le, L, R}) ->
    evals(OtherAtts,L) ++ " < " ++ evals(OtherAtts,R);
evals(OtherAtts,{intersect, L, R}) ->
    evals(OtherAtts,L) ++ " andalso " ++ evals(OtherAtts,R);
evals(OtherAtts,{union, L, R}) ->
    evals(OtherAtts,L) ++ " orelse " ++ evals(OtherAtts,R);
evals(OtherAtts,{ismember, L, R}) ->
    L1= evals(OtherAtts,L),
    R1 = evals(OtherAtts,R),
    "sets:is_element(" ++ L1 ++ "," ++ R1 ++ ")";
evals(OtherAtts,{notmember, L, R}) ->
    "not " ++ evals(OtherAtts,{ismember, L, R});
evals(OtherAtts,{negation, T}) ->
    " not " ++ evals(OtherAtts,T).


%% evaluation of eval receive predicate
evalr(OtherAtts,{parenthesis,P},M) ->
    "(" ++ evalr(OtherAtts,P,M) ++ ")";
evalr(OtherAtts,{bracket,L},M) ->
    "[" ++ string:join([evalr(OtherAtts,E,M) || E <- L], ",") ++ "]";
evalr(OtherAtts,{bracket2,L}, M) ->
    "sets:from_list([" ++ string:join([evalr(OtherAtts,E,M) || E <- L], ",") ++ "])";
evalr(OtherAtts,{head,N},M) -> evalr(OtherAtts,N,M) ++ ".head";
evalr(OtherAtts,{tail,N},M) -> evalr(OtherAtts,N,M) ++ ".tail";

%% NEED TO REVIEW
evalr(_,"true", _) -> "true";
evalr(_,"false", _) -> "false";
evalr(_,{self,Att},_) -> "att(" ++ Att ++ ",_LclE)";
evalr(_,{param,Att},_) -> "var(" ++ Att ++ ",_Var)";
evalr(OtherAtts,{'+',L,R},M) ->
    evalr(OtherAtts,L,M) ++ " + " ++ evalr(OtherAtts,R,M);
evalr(OtherAtts,{'-',L,R},M) ->
    evalr(OtherAtts,L,M) ++ " - " ++ evalr(OtherAtts,R,M);
evalr(OtherAtts,{'*',L,R},M) ->
    evalr(OtherAtts,L,M) ++ " * " ++ evalr(OtherAtts,R,M);
evalr(OtherAtts,{const,C},_) -> C;
evalr(OtherAtts,{minusconst,C}, _) -> "-" ++ C;
evalr(OtherAtts,[],_) -> "[]";
evalr(OtherAtts,{token,T},_) ->
    lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evalr(OtherAtts,[{self,Att}=Term], M) ->
    "[" ++ evalr(OtherAtts,Term, M) ++ "]";
evalr(OtherAtts,[H|T]=List,M) when T =/= [] ->
    S = [evalr(OtherAtts,Name,M) || Name <- List],
    "[" ++ string:join(S,",") ++ "]";
evalr(OtherAtts,{eq, L, R}, M) ->
    evalr(OtherAtts,L, M) ++ " == " ++ evalr(OtherAtts,R, M);
evalr(OtherAtts,{diff, L, R}, M) ->
    evalr(OtherAtts,L, M) ++ " =/= " ++ evalr(OtherAtts,R,M);
evalr(OtherAtts,{ge, L, R}, M) ->
    evalr(OtherAtts,L, M) ++ " > " ++ evalr(OtherAtts,R, M);
evalr(OtherAtts,{geq, L, R}, M) ->
    evalr(OtherAtts,L, M) ++ " >= " ++ evalr(OtherAtts,R, M);
evalr(OtherAtts,{le, L, R}, M) ->
    evalr(OtherAtts,L, M) ++ " < " ++ evalr(OtherAtts,R, M);
evalr(OtherAtts,{leq, L, R}, M) ->
    evalr(OtherAtts,L, M) ++ " =< " ++ evalr(OtherAtts,R,M);
evalr(OtherAtts,{intersect, L, R},M) ->
    evalr(OtherAtts,L,M) ++ " andalso " ++ evalr(OtherAtts,R, M);
evalr(OtherAtts,{union, L, R}, M) ->
    evalr(OtherAtts,L,M) ++ " orelse " ++ evalr(OtherAtts,R, M);
evalr(OtherAtts,{notmember, L, R}, M) ->
    L1= evalr(OtherAtts,L, M),
    R1 = evalr(OtherAtts,R,M),
    "not sets:is_element(" ++ L1 ++ "," ++ R1 ++ ")";
evalr(OtherAtts,{ismember, L, R},M) ->
    L1= evalr(OtherAtts,L,M),
    R1 = evalr(OtherAtts,R,M),
    "sets:is_element(" ++ L1 ++ "," ++ R1 ++ ")";
evalr(OtherAtts,{negation, T},M) ->
    " not " ++ evalr(OtherAtts,T,M);
evalr(OtherAtts,{literal,Name}, M) ->
    io:format("verify ~p in ~p ~n",[Name,OtherAtts]),
    case lists:member(Name,OtherAtts) of
	true ->
	    "att(" ++ Name ++ ",_RmtE)";
	false ->
	    case proplists:get_value(Name,M) of
		undefined ->
		    "var(" ++ lists:flatten(io_lib:format("~p",[list_to_atom(Name)])) ++ ",V)";
		    %"_" ++  string:uppercase(Name);
		I ->
		    "msg(" ++ integer_to_list(I) ++ ",_M)"
	    end
    end.

build_msg(Exps) ->
    build_msg(Exps,[]).

build_msg(empty,S) ->
    "{}";
build_msg([],S) -> %
    Msg = "{" ++ string:join(lists:reverse(S),",") ++ "}",
    Msg;
build_msg([H|T], S) ->
    build_msg(T,["fun(_LclE) -> " ++ evale(H) ++ " end"|S]).

%% TODO: UMC does not support sending compound expressions
%% Thus need to represent them as temporal variables

evale({'+',L,R}) -> evale(L) ++ " + " ++ evale(R);
evale({'*',L,R}) -> evale(L) ++ " * " ++ evale(R);
evale({'-',L,R}) -> evale(L) ++ " - " ++ evale(R);
evale({head,Name}) -> evale(Name) ++ ".head";
evale({tail,Name}) -> evale(Name) ++ ".tail";
evale({self,Att}) -> "att("++ Att ++ ",_LclE)";
evale({literal,T}) ->
      "var(" ++ lists:flatten(io_lib:format("~p",[list_to_atom(T)])) ++ ",V)";
%    "_" ++ string:uppercase(T);
evale({token,T}) -> lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evale({const,C}) -> C;
evale({minusconst,C}) -> "-" ++ C;
evale({func,Name,L}) -> "user_code:" ++ Name ++ "(" ++ string:join([evale(C) || C <- L],",") ++ ")".
