-module(readfile).
-export([read_data/1, check_color/2]).


read_data(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinData = binary:split(Data, [<<"\n">>], [global]),
%    io:format("BinData ~p ~n", [BinData]),
    extract(BinData).




extract(BinData) ->
%   io:format("~p~n",[BinData]),
    extract(BinData, #{}, 0).

extract([], Map, C) ->
    io:format("number of vertices ~p, read in ~p vertices ~n",[C,length(maps:keys(Map))]),
    ok = check_data(Map),
    Map;
extract([H|T], Map, C) when is_binary(H) ->
 %   io:format("~p ~n",[H]),
    extract([string:lexemes(H," ")|T], Map, C);
extract([[<<E>>, S, D] |T], Map, C) when E == $e ->
%   io:format("gets ~p to ~p ~n",[S,D]),
    New = maps:update_with(binary_to_integer(S), fun(V) -> lists:usort([binary_to_integer(D)|V]) end, [binary_to_integer(D)], Map),
    New2 = maps:update_with(binary_to_integer(D), fun(V) -> lists:usort([binary_to_integer(S)|V]) end, [binary_to_integer(S)], New),
    extract(T,New2, C);
extract([[<<P>>, ED, N, E] |T], Map, C) when P == $p ->
    extract(T,Map,binary_to_integer(N));
extract([_|T], Map, C) ->
    extract(T,Map, C).

check_data(Map) ->
    check(maps:iterator(Map),Map).

check(none,_) -> ok;
check(I,Map) ->
    {K1, V1, I2} = maps:next(I),
    %% check for K1
    if is_list(V1) ->
	    N = length(V1),
	    N == length([X || X <- V1, lists:member(K1, maps:get(X,Map))]);
       true ->
	    exit(self(),"problem")
    end,
    check(I2,Map).


check_color(CL, VMap) ->
    CMap = maps:from_list(CL),
    check_color(maps:iterator(CMap), CMap, VMap).

check_color(none,_, _) -> ok;
check_color(I, CMap, VMap) ->
    {K1, V1, I2} = maps:next(I),
    Neighbors = maps:get(K1, VMap),
    NColors = [maps:get(X,CMap) || X <- Neighbors],
    case lists:member(V1, NColors) of
	true ->
	    io:format("Error of assinging vertex ~p with color ~p~n",[K1,V1]);
	false ->
	    ok
    end,
    check_color(I2,CMap,VMap).
