-module(abc2abel).
-export([file/1, main/1, view/1, flatten/1]).
-define(FS,"    ").
-record(state,{aware, unfold, params}).

print(X) -> io:format("~p~n", [X]).

%% build the project
main(["build"]) ->
	leex:file(scanner),
	yecc:file(parser);
main([Fname]) -> file(Fname);
main(["scan", Fname]) ->
	{ok, Binary} = file:read_file(Fname),
	print(scanner:string(binary_to_list(Binary))).

%% run a file name
file([Atom]) when is_atom(Atom) ->
    file(atom_to_list(Atom) ++ ".abc");
file(Fname) ->
    {ok, Binary} = file:read_file(Fname),
    trans(binary_to_list(Binary)).

%% For viewing the AST
view([Atom]) when is_atom(Atom) ->
    view(atom_to_list(Atom) ++ ".abc");
view(Fname) ->
    {ok, Binary} = file:read_file(Fname),
    view1(binary_to_list(Binary)).
view1(Code) ->
    {ok, Tree} = parser:scan_and_parse(Code),
    io:format("~p~n",[Tree]).

%% translate the code
trans(String) when is_list(String) ->
    {ok, Tree} = parser:scan_and_parse(String), % get AST

    %% extract necessary system structure
    Sys = [Y || {sys, Y} <- Tree], % Considering those components are declared under SYS
    CompDefs = [X || X <- Tree, element(1,X) == comp], % component definitions
    Comp_inits = [X || X <- Tree, element(1,X) == comp_init], % component instantiations
    CNames = [X || {comp, X, _} <- CompDefs], % component names

    catch ets:delete(abcsystem),
    catch ets:delete(counter),
    catch ets:delete(auxilary),
    catch ets:delete(umcsystem),

    ets:new(counter,[named_table]),
    ets:new(auxilary,[named_table]),
    ets:new(abcsystem,[named_table]), %% the table that keeps all
    %% Store code definitions
    preprocessing(CompDefs, Comp_inits), % FULL OF SIDE-EFFECTS!
    AttEnv = ets:lookup_element(abcsystem, attributes, 2),
    ets:new(umcsystem,[named_table]), %% the table that store trasnlated code
    ets:insert(umcsystem,{allmodules, CNames}),
    body(CNames). % the main work

%% translate each component type!
body([CName|Rest]) ->
    IExport = "-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).\n-export([init_beh/2]).\n",
    Header = "-module(" ++ lower_case(CName) ++ ").\n" ++ IExport,
    ets:insert(CName,{body,[]}),
    ets:insert(CName, {header, Header}),
    ets:insert(counter,{c,0}),
    ets:insert(auxilary,{beh,[]}),

    Process = #{ proc_name => CName},
    ets:insert(CName, {visited, []}), % the init process have the same name as component name

    AllDefs = ets:lookup_element(CName,beh,2),

    [call(Def,CName) || Def <- AllDefs],
    gen_aux(CName),

    CodeForCName = lists:reverse(ets:lookup_element(CName,body,2)),
    io:format("Normalization ~p ~n",[CodeForCName]),
    %% map code to printable erlang code
    %% only those needed -> find dependency
    DepsCode = chain(lists:map(fun({def, P, _, {prefix, _, {_,K,_}}}) ->
				   % genrate code for prix, Con is a process call
    				   {P,[K]};
    			      ({def,P, _, {choice,_,_} = Code}) ->
    				   {P,[K || {prefix,_,{_,K,_}} <- flatten(Code)]};
    			      ({def,P, _, {par,_,_} = Code}) ->
    				    {P,[X || {_,X,_} <- flatten(Code)]};
    			      ({def,P, _, {call,K,_}}) ->
    				   {P,[K]};
    			      (Other) ->
    				   {ok,[]}
    			   end,
    			   CodeForCName)),
    Need = lists:filter(fun(X) ->
			        lists:member(element(2,X), DepsCode)
			end,
			CodeForCName),

%    io:format("===== Deps code ~p and ~p ~n",[DepsCode, Need]),

    io:format("Normalization ~p ~n",[Need]),

    ErlangCode = lists:map(fun({def, P, Args, {prefix, Act, Con}}) ->
				   % genrate code for prix, Con is a process call
    				   def(P,Args) ++ "    prefix(_C,V,{" ++ Act ++ ",\n\t" ++ ref(Con) ++ "}\n\t).\n\n";
    			      ({def,P, Args, {choice,_,_} = Code}) ->
    				   def(P,Args) ++ "    choice(_C,V,[\n\t" ++ string:join(["{" ++ Act ++ ",\n\t" ++ ref(Con) ++ "}"  || {prefix,Act,Con} <- flatten(Code)],",\n\t") ++ "\n    ]).\n\n";
    			      ({def,P, Args, {par,_,_} = Code}) ->
				   %io:format("Def par for ~p ~n",[Code]),
    				   def(P,Args) ++ "    parallel(_C,V,[\n\t" ++ string:join([ref_par(X,Args) || X <- flatten(Code)],",\n\t") ++ "\n    ]).\n\n";
    			      ({def,P, Args, {call,K,_} = Code}) ->
    				   def(P,Args) ++ "    call(_C,V," ++ ref(Code,Args) ++ ").\n\n";
    			      (Other) ->
				   io:format("Other does not match ~p ~n",[Other]),
    				   Other
    			   end,
    			   Need),
    io:format("===== AFTER code ~p ~n",[ErlangCode]),
    ets:insert(CName,{body,ErlangCode}),
    body(Rest);
body([]) ->
    %% Print = "\n %% COMPONENT BEHAVIOUR " ++ atom_to_list(CName) ++ "\n",
    %% output_abel(CName,Print),
    AllNames = ets:lookup_element(umcsystem,allmodules,2),
    [file:write_file(lower_case(CName) ++ ".erl", [ets:lookup_element(CName,header,2), ets:lookup_element(CName,body,2)], [append]) || CName <- AllNames].

%% Helper functions
%% Entry = {[parent process],entry value for transition}
call(Code, CName) ->
    %% carry out the information as how to produce code for an action
    ActionState = #state{aware = [], % no awareness yet
			 unfold = false, %true to unfold the choice
			 params = [] % formal params
			},
    eval(Code, CName, ActionState).

eval({proc,Args,Code}, CName, ActionState) ->
    eval(Code,CName, ActionState#state{params = Args});

eval({def, ProcName, Args, Code}, CName, ActionState) ->
    R = {def, ProcName, Args, eval(Code, CName, ActionState#state{params = Args})},
    print(CName,R);

eval({p_awareness, Pred, Process}, CName, ActionState) ->
    eval(Process, CName, ActionState#state{aware = Pred});

%% Prefixing process, con is a process call
eval({prefix, Left, {call, ProcName, Args} = Right}, CName, ActionState) ->
    Act = build_act(Left, CName, ActionState),
    R = {prefix, Act, eval(Right, CName, ActionState#state{unfold = false})},
    R;
%% Prefixing, con is process expessions
eval({prefix, Left, Right}, CName, ActionState = #state{params = Args}) -> % Right is a complex process
    %% creating code for this action, and then proceeding with ProcName
    R = if Right =/= nil ->
		Act = build_act(Left, CName, ActionState),
		Con = fresh(),
%		io:format("What is parameter for newly created ~p (~p) ~n",[Con, Args ++ Vars]),
		auxilary({def, Con, Args, Right}),
		{prefix, Act, {call_new, Con, Args}};
	   true ->
		Act = build_act(Left, CName, ActionState),
		{prefix,Act,{call,nil,[]}}
    end,
    R;

%% the execution model : call a process, then trace the proc definition to unfold
eval({call, ProcName, Args}, CName, ActionState = #state{unfold = UF}) ->
    R = if UF -> %unfolding the code as required from some choice command
		Code = ets:lookup_element(CName, ProcName, 2),
		eval(Code,CName,ActionState#state{unfold=false});
	   true ->
		{call, ProcName, Args}
	end,
    %io:format("return ~p ~n",[R]),
    R;

eval({choice,P,Q}, CName, ActionState) ->
    R = {choice, eval(P, CName, ActionState#state{unfold=true}),
	 eval(Q, CName, ActionState#state{unfold=true})},
    R;

eval({par, P, Q}, CName, ActionState = #state{params = Args}) ->
    io:format("eval par ~p ~p ~n",[P,Q]),
    L = if element(1,P) == call orelse element(1,P) == par ->
		eval(P, CName, ActionState);
	   true ->
		L1 = fresh(), % create fresh proc name
		auxilary({def,L1,Args,P}),
		{call_new, L1, []}
	end,
    R = if element(1,Q) == call orelse element(1,Q) == par  ->
		eval(Q, CName, ActionState);
	   true ->
		R1 = fresh(),
		auxilary({def,R1,Args,Q}),
		{call_new, R1, []}
	end,
    Ret = {par, L, R},
    Ret;


%% should never get in here
eval(FINAL, CName, ActionState) ->
    io:format("FINAL DOES NOT MATCH, ~p ~p ~p ",[FINAL, CName, ActionState]).


%% get specifications of actions
build_act({{output, Exps, Pred}, Upd}, CName, #state{aware = Aware, params = Params}) ->
    MyAtts = my_attrs(CName),
    OtherAtts = other_attrs(CName),
    G = utils:build_apred(MyAtts,Aware),
    M = utils:build_msg(Exps),
    U = utils:build_update(MyAtts,Upd,[]),
    SP = utils:build_spred(OtherAtts,Pred),
    Act = "{" ++ string:join([G,M,SP,U],",\n\t") ++ "}"; % empty set of vars
build_act({{input, Pred, Vars}, Upd}, CName, #state{aware = Aware, params = Params}) ->
    MyAtts = my_attrs(CName),
    OtherAtts = other_attrs(CName),
    Msg = bound_assignment(Vars),
    X = "{" ++ string:join(Vars,",") ++ "}",
    G = utils:build_apred(MyAtts,Aware),
    U = utils:build_update(MyAtts,Upd,Msg),
    %io:format("My Atts ~p, Other Atts ~p~n",[MyAtts,OtherAtts]),
    RP = utils:build_rpred(OtherAtts,Pred,Msg),
    Act = "{" ++ string:join([G,RP,X,U],",\n\t") ++ "}".


preprocessing(CompDefs, Comp_inits) ->
    ets:insert(abcsystem, {attributes, #{}}),
    %% component indexes
    I = lists:seq(0,length(CompDefs) - 1),
    Comps = lists:zip(I, CompDefs),
    %% storing components behaviour definitions and initializing them into each own table
    lists:map(fun({I1, {comp, CName, [{attrs, AttList}, {obsrs, ObsList}, {beh, Behaviour, {_,Init}}]}}) -> % FULL OF SIDE-EFFECT
		      %% each component has an information table
		      catch ets:delete(CName),
		      ets:new(CName, [named_table]),
		      %% store the init behaviour of component definition CName
		      ets:insert(CName, {obsrs, ObsList}),
		      InitBeh = {def, init_beh, [], Init},
		      %% now store also all beh
		      ets:insert(CName, {beh, [InitBeh | Behaviour]}),

		      %% store process definitions of this component
		      Fun = fun({def, ProcName, Args, Code}) ->
				    ets:insert(CName, {ProcName, {proc, Args, Code}})
				    %ets:insert(CName, {ProcName, {def, ProcName, Args, Code}})
			    end,
		      lists:map(Fun, Behaviour),
		      %% Create the attributes list from component definitions
		      %% Assigning component indexes to each attribute name
		      Acc = ets:lookup_element(abcsystem, attributes, 2),
		      Map = maps:put(CName, AttList, Acc),
		      ets:insert(abcsystem, {attributes, Map})
	      end, Comps),
    io:format("Set of attribute collected is ~p~n",[ets:lookup_element(abcsystem, attributes, 2)]),
    %% Collect attribute environments of all compnents
    Data = lists:foldl(fun({comp_init, CInit, {comp, _, Decl} = Body}, Acc) ->
			       ets:insert(abcsystem, {CInit, Body}),
			       [Decl | Acc]
		       end, [], Comp_inits),
    %% keep initialization data for attribute environments to output to the UMC model
    ets:insert(abcsystem, {data, Data}).


data_eval({token,T}) ->
    L = get(token),
    put(token, L#{T => T}),
    T;
data_eval({const,C}) ->
    C;
data_eval({minusconst,C}) ->
    "-" ++ C;
data_eval(empty_vector) ->
    "[]";
% an array of single elements
data_eval({bracket, List}) when is_list(List) ->
   % io:format("eval for ~p ~n",[List]),
    Array = [data_eval(X) || X <- List],
    "[" ++ string:join(Array,",") ++ "]";
data_eval({bracket, E}) ->
    "[" ++ data_eval(E) ++ "]";
data_eval(Other) -> Other.

filter_bindings(Args, VarNames) -> % vars is the message, bindings is
    %% simplest case, Vars \in Bindings
    Name = [X || {_,X} <- Args],
    L = [{X,proplists:get_value(X,VarNames)} || X <- Name].

select_bindings(FormalArgs, ActualArgs) ->
    Vals = [I || {_,I} <- ActualArgs],
    lists:zip(FormalArgs,Vals).

%% previous vars [{x,0}, {y,1}, ...]
%% new vars  = [x, z,t,w],
%% get several things: assign variable in the message to bound, return new proplist of [{allvars , indexes}] and the message
bound_assignment(Vars) ->
%    io:format("previous vars empty ~n",[]),
    Index1 = lists:seq(1, length(Vars)),
    Msg = lists:zip(Vars, Index1),
    Msg.


output_abel(Name,Code) ->
    Body = ets:lookup_element(Name,body,2),
    ets:insert(Name, {body, [Code|Body]}).

auxilary(Code) ->
    Body = ets:lookup_element(auxilary,beh,2),
    ets:insert(auxilary, {beh, [Code|Body]}).

lower_case(T) ->
    lists:flatten(io_lib:format("~p",[list_to_atom(string:lowercase(atom_to_list(T)))])).


def(Name, Args) when length(Args) == 0 ->
    %io:format("param list for ~p (~p) ~n", [Name,Args]),
    F = lower_case(Name),
    F ++ "(_C,V) -> \n";
def(Name,Args) when length(Args) > 0 ->
    %io:format("param list for ~p (~p) ~n", [Name,Args]),
    F = lower_case(Name),
    case if_new(F) of
	false ->
	    A = string:join(lists:map(fun(X) -> "_" ++ string:uppercase(X) end, Args), ","),
	    F ++ "(_C,V," ++ A ++ ") -> \n";
	true ->
	    A = string:join(lists:map(fun(X) -> "_" ++ string:uppercase(X) end, Args), ","),
	    F ++ "(_C,V,[" ++ A ++ "]) -> \n"
    end.


ref({call,nil,_}) ->
    "nil";
ref({call,Name,A1}) when A1 =/= [] ->
    P = lower_case(Name),
    "fun(_V) -> " ++ P ++ "(_C,_V) end";
ref({call,Name,A1}) when A1 == [] ->
    P = lower_case(Name),
    "fun(_V) -> " ++ P ++ "(_C,[]) end";
ref({call_new,Name,_}) ->
    P = lower_case(Name),
    "fun(_V) -> " ++ P ++ "(_C,_V) end".




ref({call,Name,A1},M) ->
    %io:format("param list for ~p (A1 = ~p , M = ~p) ~n", [Name,A1,M]),
    ref_nom(Name,A1,M);
ref({call_new,Name,A1},M) ->
    %io:format("param list for ~p (A1 = ~p , M = ~p) ~n", [Name,A1,M]),
    ref_new(Name,A1,M).


ref_nom(nil,_,_) ->
    "nil";
ref_nom(Name,A1,A2) when A1 == [] ->
    P = lower_case(Name),
    "fun(_V) -> " ++ P ++ "(_C,_V) end";
ref_nom(Name,A1,A2) when A1 =/= [] ->
    P = lower_case(Name),
    A = utils:build_args(A1,A2),
    "fun(_V) -> " ++ P ++ "(_C,_V," ++ A ++ ") end".

ref_new(Name,A1,A2)  ->
    io:format("ref new for ~p with ~p ~p ~n",[Name,A1,A2]),
    P = lower_case(Name),
    Closure = if A1 == [] -> "";
		 true ->
		      "," ++ string:join(lists:map(fun(X) -> "_" ++ string:uppercase(X) end, A1), ",")
	      end,
    if A2 == [] ->
	    "fun(_V) -> " ++ P ++ "(_C,_V" ++ Closure ++  ") end";
       true ->
	    Index = lists:seq(1,length(A2)),
	    A = "," ++ string:join(lists:map(fun(I) -> "msg(" ++ integer_to_list(I) ++ ",_M)" end, Index), ","),
	    "fun(_V) -> " ++ P ++ "(_C,_V" ++ Closure ++ A ++ ") end"
    end.
%% ref_new(Name,A1,A2) when A1 =/= [] ->
%%     P = lower_case(Name),
%%     A = utils:build_args(A1,A2),
%%     "fun(_M) -> " ++ P ++ "(_C," ++ A ++ ") end".

ref_par({call,Name,A1},M) ->
    %io:format("param list for ~p (A1 = ~p , M = ~p) ~n", [Name,A1,M]),
    ref_nom(Name,A1,M);
ref_par({call_new,Name,A1},M) ->
    %io:format("param list for ~p (A1 = ~p , M = ~p) ~n", [Name,A1,M]),
    ref_par_new(Name,A1,M).

ref_par_new(Name,A1,A2) when A1 == [] ->
    P = lower_case(Name),
    if A2 == [] ->  "fun(_V) -> " ++ P ++ "(_C,_V) end";
       true -> "fun(_V) -> " ++ P ++ "(_C,_V," ++ string:join(lists:map(fun(X) -> "_" ++ string:uppercase(X) end, A2), ",") ++ ") end"
    end;
ref_par_new(Name,A1,A2) when A1 =/= [] ->
    P = lower_case(Name),
    %io:format("Build args for ~p where  A = ~p, msg = ~p ~n",[Name,A1,A2]),
    A = utils:build_args(A1,A2),
    "fun(_V) -> " ++ P ++ "(_C,_V," ++ A ++ ") end".

%%% choice choice choice
flatten({OP, L, R})->
    flatten({OP, L, R},[]).

flatten({OP, {OP,_,_}=L, R}, Acc) ->
    flatten(L,[R | Acc]);
flatten({OP,L,R},Acc) ->
    lists:flatten([L,R | Acc]).


print(CName,R) ->
    output_abel(CName,R),
    ok.
    %io:format("R = ~p~n",[R]).

fresh() ->
    C = ets:update_counter(counter,c,1),
    F = "_fun" ++ integer_to_list(C),
    list_to_atom(F).


gen_aux(CName) ->
    case ets:lookup_element(auxilary,beh,2) of
	[] -> ok;
	AllAuxs ->
	    ets:insert(auxilary,{beh,[]}),
	    [call(Aux,CName) || Aux <- AllAuxs],
	    gen_aux(CName)
    end.


other_attrs(CName) ->
    %% other attributes should be all attributes
    Attrs = ets:lookup_element(abcsystem, attributes, 2),
    Others = lists:append([X || {O,X} <- maps:to_list(Attrs)]).

my_attrs(CName) ->
    Attrs = ets:lookup_element(abcsystem, attributes, 2),
    My = lists:append([X || {O,X} <- maps:to_list(Attrs), O == CName]).


if_new(Name) ->
    if_new("_fun",Name).

if_new(Prefix,Name) when length(Name) > length(Prefix) ->
    {Prefix1,_} = lists:split(length(Prefix),Name),
    Prefix == Prefix1;
if_new(Prefix,Name) ->
    false.


chain(L) ->
    chain([init_beh],L,[]).

chain([H|T],L,Acc) ->
    %io:format("chain for ~p where ~p ~n",[H,Acc]),
    S = unique(proplists:get_value(H,L)),
    S2 = S -- Acc,
    case lists:member(H,Acc) of
	true ->
	    chain(S2 ++ T,L,Acc);
	false ->
	    chain(S2 ++ T,L,[H|Acc])
    end;
chain([],_,Acc) ->
    lists:reverse(Acc).

unique([H|T]) -> [H | [X || X <- unique(T), X =/= H]];
unique([]) -> [];
unique(_U) -> []. %% needed because of the name nil
