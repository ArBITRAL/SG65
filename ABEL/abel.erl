%%% interface module
-module(abel).

-export([start/1,start/2]).

-export([new_component/3,start_component/1]).

-export([choice/3,parallel/3,prefix/3,call/3,var/2,att/2,msg/2]).

%% Infrastructure
start(T) ->
    start(T,binary).

start(T,Mode) ->
    abel_sup:start_link(T), % start the infrastructure
    ok = abel_reg:create_tree(Mode).  % shape a binary tree of T nodes

%% Components
new_component(M, Env, I) ->
    {ok, C} = abel_coord:new_component(M, Env, I),
    C. %separate this two for handling massive initializations

start_component(C) ->
    abel_coord:start_component(C).

prefix(C,V,{Act,Con}) ->
    SomeAct = pretty_format(Act),
    {F,A} = abel_coord:prefix(C, {SomeAct, Con}),
    A2 = append(A,V),
    app(F,A2).

%% chocice
choice(C, V, BehList) ->
    BList2 = [{pretty_format(Act),Con} || {Act,Con} <- BehList],
    {F,A} = abel_coord:choice(C,BList2),
    A2 = append(A,V),
    app(F,A2).

%% interleaving
parallel(C, V, L) when is_list(L) ->
    L2 = [fun() -> F(V) end || F <- L],
    abel_coord:par(C, L2).

call(_C,V,F) ->
    F(V).

app(nil,_) ->
    nil;
app(F,A) ->
    F(A).

%% new bound variables
append([], L) -> L;
append(X, L) ->
    F = fun({K,_} = Y,Acc) ->
		case proplists:is_defined(K,Acc) of
		    false ->
			[Y | Acc];
		    true ->
			lists:keyreplace(K, 1, Acc, Y)
		end
	end,
    lists:foldl(F,X,L).


msg(I,M) ->
    element(I,M).

var(X,Var) ->
    proplists:get_value(X,Var).

att(A,E) ->
    maps:get(A,E).

%% internal formatting functions
pretty_format({Msg,Pred}) when is_tuple(Msg) andalso is_function(Pred) ->
    {send, fun(_) -> true end, Msg, Pred, []};
pretty_format({G,Msg,Pred}) when is_function(G) andalso is_function(Pred) andalso is_tuple(Msg) ->
    {send, G, Msg, Pred, []};
pretty_format({Msg,Pred,U}) when is_tuple(Msg) andalso is_function(Pred) andalso is_list(U) ->
    {send, fun(_) -> true end, Msg, Pred, U};
pretty_format({G,Msg,Pred,U}) when is_function(G) andalso is_tuple(Msg) andalso is_function(Pred) andalso is_list(U) ->
    {send,G,Msg,Pred,U};
pretty_format({Pred,X}) when is_function(Pred) andalso is_tuple(X) ->
    {recv, fun(_) -> true end, Pred, X, []};
pretty_format({G,Pred,X}) when is_function(G) andalso is_function(Pred) andalso is_tuple(X) ->
    {recv, G, Pred, X, []};
pretty_format({Pred,X,U}) when is_function(Pred) andalso is_tuple(X) andalso is_list(U) ->
    {recv, fun(_) -> true end, Pred, X, U};
pretty_format({G,Pred,X,U}) when is_function(G) andalso is_function(Pred) andalso is_tuple(X) andalso is_list(U)  ->
    {recv,G,Pred,X,U}.
