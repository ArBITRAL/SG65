-module(dcolor).

-import(abel,[new_component/3,start_component/1]).

-export([start/2, start/1]).

start(N) ->
    Name = "heawood.txt",
    start(Name,N).

start(FileName,N) ->
    ok = abel:start(N),
    Input = readfile:read_data(FileName),
    Data = build_input(Input),
    VertexNum = length(Data),
    Common = #{report => self() , colour => 0, round => 0, done => 0, send => 1, assigned => 0, used => sets:new(), counter => 0, constraints => sets:new()},
    I = {id},

    ets:new(colour,[named_table,public]),
    ets:new(round,[named_table,public]),
    ets:insert(round,{num,0}),

    CompAddresses = lists:foldr(fun(X, Acc) ->
					Env = maps:merge(X,Common),
					Comp = new_component(vertex, Env, I),
					[Comp | Acc]
				end, [], Data),
    %% Start execution
    Start1 = os:timestamp(),
    [start_component(C) || C <- CompAddresses],

    [Colour, Round, Outcome] = collect_results(VertexNum),
    io:format("colour=~p~n", [Colour]),
    io:format("round=~p~n", [Round]),
    io:format("Computation time=~p seconds~n",[timer:now_diff(os:timestamp(), Start1)/1000000]),
    io:format("~n"),
    ok = readfile:check_color(Outcome,Input).


collect_results(0) ->
    Round = ets:lookup_element(round,num,2),
    CList = ets:tab2list(colour),
    Colour = lists:max([C || {_,C} <- CList]),
    [Colour, Round, CList];
collect_results(VertexNum) ->
    receive
	{done, Id, Colour, Round} ->
	    io:format("(~p, ~p, ~p) ~n",[Id,Colour,Round]),
	    ets:insert(colour,{Id,Colour}),
	    ets:insert(round,{num,max(Round,ets:lookup_element(round,num,2))}),
	    collect_results(VertexNum - 1)
    end.

build_input(M) ->
    build_input(maps:iterator(M), []).

build_input(none, Acc) -> Acc;
build_input(I, Acc) ->
    {K1, V1, I2} = maps:next(I),
    build_input(I2, [#{id => K1, nbr => sets:from_list(V1)} | Acc]).
