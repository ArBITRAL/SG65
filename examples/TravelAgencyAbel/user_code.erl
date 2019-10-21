-module(user_code).
-compile(export_all).

%% helper functions
selector1(L,I) ->
    proplists:get_value(I,L).

selector2(L,I,J) ->
    L1 = proplists:get_value(I,L),
    proplists:get_value(J,L1).

zero(C,I) ->
    lists:keyreplace(I,1,C,{I,0}).

inc(C,I) ->
    I1 = proplists:get_value(I,C) + 1,
    lists:keyreplace(I,1,C,{I,I1}).

dec(C,I) ->
    I1 = proplists:get_value(I,C) - 1,
    lists:keyreplace(I,1,C,{I,I1}).


distance(L1,L2) ->
    abs(L1 - L2).
