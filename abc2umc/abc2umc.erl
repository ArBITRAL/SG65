-module(abc2umc).
-export([file/1, main/1, view/1]).

-record(state,{parent, entry, aware, exit, pc_index, pc_name, bound, nil, schedule}).

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
    ets:new(abcsystem,[named_table]), %% the table that keeps all
    %% Store code definitions
    preprocessing(CompDefs, Comp_inits), % FULL OF SIDE-EFFECTS!

    AttEnv = ets:lookup_element(abcsystem, attributes, 2),
    %print(AttEnv),
    %% Translation
    catch ets:delete(umcsystem),
    ets:new(umcsystem,[named_table]), %% the table that store trasnlated code
    header(AttEnv),    % producing the header UMC model
    body(CNames), % the main work, producing the body of the model
    finish(Comp_inits, Sys).    % producing the rest of the model

header(AttEnv) ->
    Class = "Class System with niceparallelism is\nSignals:\n\tallowsend(i:int);\n\tbroadcast(tgt,msg,j:int);\nVars:\n\tRANDOMQUEUE;\n\treceiving:bool := false;\n\tpc :int[];\n\tbound:int[];\n\t\n\t-- attributes\n\t" ++ string:join(maps:keys(AttEnv),";\n\t") ++ ";\nState Top Defers allowsend(i)",
    ets:insert(umcsystem,{header, Class}),
    ets:insert(umcsystem,{body,[]}).
%    file:write_file("foo.umc", [Class, Trans, io_lib:nl()], [append]).

%% translate each component type!
body([CName|Rest]) ->
    InitBehaviour = ets:lookup_element(CName, init_beh, 2), % gets the init behaviour of component
    ParvectorName = atom_to_list(CName) ++ "par_vector",
    put(ParvectorName, []),
    % storing the number of processes in this component type, abusing for determining process index init = 0
    ets:insert(CName, {num_procs, 0}),

    %% main process index and counter
    %% force using Component name as process name in the begining
    %% Note that bound index is different from Pc index, they increase their values differently
    %% there is no parent in the first time
    ProcessState = CName,
    Process = #{parent => {},
		proc_name => CName,
		v_name => [], % proplist contain variables in the received message and its index
		%% bound => [], % same above but containing only those variables passed to the process call
		%% arg_bindings => [], % same above but containing only those variables passed to the process call
		cnt => 1},

    ets:insert(ProcessState, {state, Process}), % representing state of a sequential process: prefixing, choice, recursion
    ets:insert(CName, {visited, []}), % the init process have the same name as component name
    Print2 = "define(" ++ atom_to_list(CName) ++ ",",
    Print = "\n\n ---------- COMPONENT TEMPLATE " ++ atom_to_list(CName) ++ " ------------ \n\n" ++ Print2,

    output_umc(Print),
    %% translate the behaviour of the component here
    call(InitBehaviour, CName, ProcessState),

    output_umc(")\n"),
    body(Rest);
body([]) -> ok. % finish translating

%% Helper functions
%% Entry = {[parent process],entry value for transition}
call(Code, CName, ProcessState) ->
    %% carry out the information as how to produce code for an action
    ActionState = #state{parent = {}, % no parent exit point
			 aware = [], % no awareness yet
			 entry = 1,   % entry point initialized as 1
			 exit = -1, % calculate exit point by itself
			 pc_index = 0,
			 pc_name = "pc[$1]",
			 nil = 0, %detech nill continuation, reset bound variables
			 schedule = [] %% for parallel copies
			},
%    io:format("translate for component ~p with code ~p and Procs State ~p ~n",[CName, Code, ProcessState]),
    eval(Code, CName, ProcessState, ActionState).


eval({proc, ProcName, Args, Code, Times}, CName, ProcessState, ActionState) when Times > 0 ->
    %%  overriden definition for translating bang
    ets:insert(CName, {ProcName, {proc, Args, Code}}),
    %% need to restore later
    %% start with no previous instances []
    eval({bang, ProcName, Args, Code, Times + 1, Times + 1, []}, CName, ProcessState, ActionState);
eval({proc, _Args, Code}, CName, ProcessState, ActionState) ->
    %%% process definition with Args list, the formal parameter may have different names compared to actual parameters in Bound
    eval(Code, CName, ProcessState, ActionState);

%% the execution model : call a process, then trace the proc definition to unfold
eval({call, ProcName, _Args}, CName, ProcessState, ActionState = #state{entry = Evalue, pc_name = PcName, pc_index = PIndex}) ->
    %% retrieve the body (code) for this function (process)
    Code = ets:lookup_element(CName, ProcName, 2),
%    io:format("ProcName ~p has code ~p, at pc_name ~p and index ~p ~n ",[ProcName, Code, PcName, PIndex]),
%    PIndex = ets:lookup_element(CName,num_procs,2),
    % v_name is a proplist [{x,0},{y,1}...] stored by some input action
    M = ets:lookup_element(ProcessState, state, 2),
%    Bindings = filter_bindings(Args, VarNames), %consider only those in Args
    %% this process is characterized by this much of information
    ProcInstance = {ProcName, PcName, PIndex},
    Visited = ets:lookup_element(CName, visited, 2),
    %% check if the process instance has already visited?
    case proplists:lookup(ProcInstance, Visited) of
	none ->
	    ets:insert(CName, {visited, [{ProcInstance, Evalue} | Visited]});
	_ ->
	    ok
    end,

   % io:format("visited so far ~p, newest index = ~p ~n",[ets:lookup_element(CName, visited, 2),PIndex]),
%    io:format("proc ~p has args ~p state ~p binding ~p ~n",[ProcName,Args,M,Bindings]),
%    ets:insert(ProcessState, {state, M#{bound => Bindings}}),
    eval(Code, CName, ProcessState, ActionState);

eval({choice,P,Q}, CName, ProcessState, ActionState) ->
    eval(P, CName, ProcessState, ActionState),
    eval(Q, CName, ProcessState, ActionState);


%% translation of parallel processes

%%% there are two cases to be considered

%% CASE 1: processes are created without a prefix action, thus, entry = 1


eval({par, P, Q}, CName, ProcessState, ActionState = #state{entry = 1}) ->
    % resuse index
    PIndex1 = ets:lookup_element(CName,num_procs,2),
  %  io:format("parallel of ~p and ~p at index = ~p ~n", [P, Q, PIndex1]),

    % creating new processState for each P and Q
    M = ets:lookup_element(ProcessState,state,2),

    Child1 = atom_to_list(CName) ++ integer_to_list(PIndex1),

    ProcessState1 = ets:new(list_to_atom(Child1),[]),
    ets:insert(ProcessState1, {state, M#{cnt := 1}}),

    eval(P, CName, ProcessState1, ActionState#state{pc_index = PIndex1}),

    ets:update_counter(CName, num_procs, 1), % bc index started from 0

    PIndex2 = ets:lookup_element(CName,num_procs,2),

    Child2 = atom_to_list(CName) ++ integer_to_list(PIndex2),
    ProcessState2 = ets:new(list_to_atom(Child2),[]),
    ets:insert(ProcessState2, {state, M#{cnt := 1}}),
    eval(Q, CName, ProcessState2, ActionState#state{pc_index = PIndex2});

%% CASE 2: processes are created with a prefix action, thus, entry = Cnt which acts as a parent exit point, entry points of all parallel processes must include their parent exit point
%% REVIEW
eval({par, P, Q}, CName, ProcessState, ActionState = #state{pc_name = Pcname, pc_index = PIndex, entry = Cnt}) ->
    ets:update_counter(CName, num_procs, 1), % bc index started from 0
    PIndex1 = ets:lookup_element(CName,num_procs,2),
%    io:format("parallel of ~p and ~p at index = ~p, entry = ~p ~n", [P, Q, PIndex,Cnt]),
    M = ets:lookup_element(ProcessState,state,2),

    Child1 = atom_to_list(CName) ++ integer_to_list(PIndex1),
    ProcessState1 = ets:new(list_to_atom(Child1),[]),
    ets:insert(ProcessState1, {state, M#{cnt := 1}}),


    eval(P, CName, ProcessState1, ActionState#state{parent = {Pcname, PIndex, Cnt}, entry = 1, pc_index = PIndex1}),
    ets:update_counter(CName, num_procs, 1), % bc index started from 0

    PIndex2 = ets:lookup_element(CName,num_procs,2),
    Child2 = atom_to_list(CName) ++ integer_to_list(PIndex2),
    ProcessState2 = ets:new(list_to_atom(Child2),[]),
    ets:insert(ProcessState2, {state, M#{cnt := 1}}),
    eval(Q, CName, ProcessState, ActionState#state{parent = {Pcname, PIndex, Cnt}, entry = 1, pc_index = PIndex2});

%% TODO : recognize P | P
%% replicate P Time times
eval({bang, ProcName, Args, Code, Times, T, _}, CName, _, _) when T =< 0 -> %stop at 0 because we counted Q in P = Q |^n P
%    io:format("translate bang at Copies = ~p, has finished ~n",[T]),
    %% update numprocs because we are not going to generate for code at index -1
    ets:update_counter(CName, num_procs, -1),
    %% restore overriden code
    ets:insert(CName, {ProcName, {proc, ProcName, Args, Code, Times - 1}}),
    %%ets:insert(CName, {ProcName, {proc, Args, Code, Times}}),
    ok;
eval({bang, ProcName, Args, Code, Total, Times, PrevInstance}, CName, ProcessState, ActionState) when Times > 0 ->
    PIndex1 = ets:lookup_element(CName,num_procs,2),
%    io:format("translate bang for ~p, at Copies = ~p, newest index ~p, previous instances index ~p ~n",[ProcName,Times,PIndex1,PrevInstance]),
    M = ets:lookup_element(ProcessState,state,2),
    Child1 = atom_to_list(CName) ++ integer_to_list(PIndex1),
    ProcessState1 = ets:new(list_to_atom(Child1),[]),
    ets:insert(ProcessState1, {state, M#{cnt := 1}}),
    eval({call, ProcName, Args}, CName, ProcessState1, ActionState#state{entry = 1, pc_index = PIndex1, schedule = PrevInstance}),
    ets:update_counter(CName, num_procs, 1),
    eval({bang, ProcName, Code, Args, Total, Times - 1, PIndex1}, CName, ProcessState, ActionState);

eval({p_awareness, Pred, Process}, CName, ProcessState, ActionState = #state{pc_index = PIndex, parent = Parent}) ->
    #{v_name := Bound} = ets:lookup_element(ProcessState,state,2),
    P = if Parent == {} -> utils:build_apred(Pred, PIndex, Bound); true -> %% awareness of first action use variable from Parent
		{_,ParentIndex,_} = Parent,
		utils:build_apred(Pred, ParentIndex, Bound)
	end,
    eval(Process, CName, ProcessState, ActionState#state{aware = [P| ActionState#state.aware]});

eval({prefix, Left, {call, ProcName, _Args} = Right}, CName, ProcessState, ActionState = #state{pc_name = PcName, pc_index = PcIndex}) ->
    ProcInstance = {ProcName, PcName, PcIndex},
    Visited = ets:lookup_element(CName, visited, 2),
    case proplists:lookup(ProcInstance, Visited) of
	{_, EntryPoint} ->
	    %% creating code for this transition, with recursion
	    create_trans(Left, ProcessState, ActionState#state{exit = EntryPoint});
	none ->
	    %% creating code for this action, and then proceeding with ProcName
	    Exit = create_trans(Left, ProcessState, ActionState),
	    %% reset the state for the next action, i.e., there no aware anymore
	    eval(Right, CName, ProcessState, ActionState#state{entry = Exit, parent = {}, aware = [], schedule = []})
    end;

eval({prefix, Left, nil}, CName, ProcessState, ActionState) -> % Right is a nil process
%    io:format("Next is nil~n"),
    %% creating code for this action, and then proceeding with ProcName
    create_trans(Left, ProcessState, ActionState#state{nil = 1}),
    ok;
eval({prefix, Left, Right}, CName, ProcessState, ActionState) -> % Right is a choice process
    %% creating code for this action, and then proceeding with ProcName
    Exit = create_trans(Left, ProcessState, ActionState),
    %% reset the state for the next action, i.e., there no aware anymore
    eval(Right, CName, ProcessState, ActionState#state{entry = Exit, parent = {}, aware = [], schedule = []});

%% may be never get in here
eval(nil, _, _, _) ->
    ok;
eval(FINAL, CName, ProcessState, ActionState = #state{pc_index = PIndex}) ->
    io:format("FINAL DOES NOT MATCH, ~p ~p ~p ",[FINAL, CName, ActionState]).



create_trans({{output, empty, empty}, Upd}, ProcessState,
	     #state{parent = Parent,
		    entry = Value,
		    aware = Aware,
		    exit = ExitPoint,
		    pc_name = Pcname,
		    pc_index = PIndex,
		   nil = Nil}) -> %% empty send ()@(ff)
    Map = ets:lookup_element(ProcessState, state, 2),
    #{cnt := Cnt, v_name := Bound}  =  Map,
    Updates = if Upd == [] -> "";
		 true ->
		      string:join(utils:build_update(Upd, PIndex, Bound), ";\n\t") ++ ";\n\t"
	      end,
    PAware = if Aware == [] -> ""; true ->
		     string:join(Aware," and ") ++ " and " end,

    Cond = utils:build_pc_guard({Parent, {Pcname, PIndex, Value}}),

    Trans = "SYS.$2.s0 -> $2.s0 {\n",
    Guards1 = "\tallowsend(i)[receiving = false and i = $1 and " ++  PAware  ++ Cond ++ "]/\n\t",

    Pc = Pcname ++ "[" ++ integer_to_list(PIndex) ++ "]",
    Exit = if ExitPoint > 0 -> ExitPoint; true -> Cnt + 1 end,

    %% reset bound if there is recursion
    Reset = if Nil > 0 orelse ExitPoint > 0 -> "\tbound[$1][" ++ integer_to_list(PIndex) ++ "] := [];\n"; true -> "" end,
    Actions1 = Updates ++ "self.allowsend($1);\n\t" ++ Pc ++ " = " ++ integer_to_list(Exit) ++ ";\n" ++ Reset ++ "}\n",
    ets:insert(ProcessState, {state, Map#{cnt := if ExitPoint > 0 -> Cnt; true -> Cnt + 1 end}}),
    Signal = " \n -----Empty Send ----- \n",
    Print = [Signal, Trans, Guards1, Actions1],%, Trans, Guards2, Actions2],
%    file:write_file("foo.umc", Print, [append]),
    output_umc(Print),
    Exit;

create_trans({{output, Exps, Pred}, Upd}, ProcessState,
	     #state{parent = Parent,
		    entry = Value,
		    aware = Aware,
		    exit = ExitPoint,
		    pc_name = Pcname,
		    pc_index = PIndex,
		    nil = Nil}) ->

    Signal = " \n ----- Send ----- ",

    Map = ets:lookup_element(ProcessState, state, 2),
    #{cnt := Cnt, v_name := Bound}  =  Map,
    PAware = if Aware == [] -> ""; true ->
		     %io:format("built _aware ~p where bound vars ~p ~n",[Aware, Bound]),
		     string:join(Aware," and ") ++ " and " end,

    Updates = if Upd == [] -> "";
		 true ->
		     % io:format("build _update ~p where bound vars ~p ~n",[Upd, Bound]),
		      string:join(utils:build_update(Upd, PIndex, Bound), ";\n\t\t") ++ ";"
	      end,
  %  io:format("update is ~p ~n",[Updates]),
    Cond1 = utils:build_pc_guard({Parent, {Pcname, PIndex, Value}}),

    Trans = "\nSYS.$2.s0 -> $2.s0 {\n",
    Guards1 = "\tallowsend(i)[receiving = false and i = $1 and " ++  PAware  ++ Cond1 ++ "]/\n",
    [Elem,Msg] = utils:build_outE(Exps, PIndex, Bound),

    Transfer = if Parent == {} ->
		       "";
		  true ->
		       {_,ParentIndex,_} = Parent,
		       "-- Transfer bound variables -- \n\tbound[$1][" ++ integer_to_list(PIndex) ++ "] := bound[$1][" ++ integer_to_list(ParentIndex) ++ "];\n\t"
	       end,

 %   io:format("msg to be send ~p ~n",[Msg]),

%    io:format("translate output ~p ~p ~p ~p ~n",[Exps, Pred, Cnt, Value]),

    [MPred,SndP] = utils:build_spred(Pred, PIndex, Bound),

    Selected = if MPred == [] -> ""; true -> "\n\t" ++ MPred end,
    %%% if there any membership predicate
    Target = "\ttarget:int[];\n\tfor j in 0..pc.length-1 {" ++ Selected ++
       "\n\tif " ++ SndP ++ " then \n\t\t{ target[j] := 1} else {target[j]:=0;}\n\t};\n",

%    Output = "\treceiving=true;\n\tif target.length > 0 then { \n\t\tself.broadcast(target," ++ Msg ++ "," ++ SIndex ++ "); OUT.sending(id[$1],"++lists:nth(1, MsgList)++");\n\t\t",

    Output = "\treceiving=true;\n\tif target.length > 0 then { \n\t\t" ++ Elem ++ "\n\t\tself.broadcast(target," ++ Msg ++ ",$1 ); \n\t\tOUT.sending($2," ++ Msg ++ ");\n\t\t",

    Exit = if ExitPoint == -1 -> Cnt+2;   % no recursion, use up to 2 values of Counter
	      true  ->  ExitPoint    %use only 1 value of the counter for the intermediate transition of the output action
	   end,
    Pc = Pcname ++ "[" ++ integer_to_list(PIndex) ++ "]",
    Guards2 = "\tbroadcast(tgt,msg,j)[" ++ Pc ++ " = " ++ integer_to_list(Cnt + 1) ++ "]/\n",
    %io:format("exit point of this action ~p~n",[Exit]),
    %% Recstring is different if there is Recursive from <pi>P or [a:=e]px
    Reset = if Nil > 0 orelse ExitPoint > 0 -> "\n\tbound[$1][" ++ integer_to_list(PIndex) ++ "] := [];"; true -> "" end,
    Reset2 = if Nil > 0 -> "\n\t" ++ Pc ++ ":= 0;"; true -> "" end,

    PReset = if Nil > 0 orelse ExitPoint > 0 -> "\n\t\tbound[$1][" ++ integer_to_list(PIndex) ++ "] := [];"; true -> "" end,
    PReset2 = if Nil > 0 -> "\n\t\t" ++ Pc ++ ":= 0;"; true -> "" end,
    PUpdates = if Updates =/= "" -> "--- attr update --- \n\t\t" ++ Updates ++ "\n\t\t"; true -> Updates end,

    Actions1 = Target ++ Output ++ PUpdates ++ Pc ++ " = " ++ integer_to_list(Cnt + 1) ++ ";\n\t} else {\n\t\t" ++ PUpdates ++
	"receiving=false;\n\t\tself.allowsend($1);\n\t\t" ++ Pc ++ " = " ++ integer_to_list(Exit) ++ ";" ++ PReset ++ PReset2 ++ "\n\t};",

    Actions2 = "\treceiving=false;\n\tself.allowsend($1);\n\t" ++ Pc ++ " = " ++ integer_to_list(Exit) ++ ";" ++ Reset ++ Reset2,
    Print = [Signal, Trans, Guards1, Transfer, Actions1, "\n}", Trans, Guards2, Actions2,"\n}"],

%    file:write_file("foo.umc", Print, [append]),
    output_umc(Print),
    NewMap = Map#{cnt => if ExitPoint == -1 -> Cnt + 2; true -> Cnt + 1 end}, % update counter accordingly, this is for another choice process to have correct program counter value
    ets:insert(ProcessState, {state, NewMap}),
    Exit;

create_trans({{input, Pred, Vars}, Upd}, ProcessState,
	     #state{parent = Parent,
		    entry = Value,
		    aware = Aware,
		    exit = ExitPoint,
		    pc_name = Pcname,
		    pc_index = PIndex,
		    nil = Nil,
		    schedule = Schedule}) ->
    Map = ets:lookup_element(ProcessState,state,2),
    #{cnt := Cnt, v_name := Bound} = Map,

    VIndex = lists:seq(0, length(Vars) - 1),
%    Vars1 = lists:zip(Vars,VIndex),

    [BoundAssignments, NewBound, Msg] = bound_assignment(Bound, Vars, integer_to_list(PIndex)),

    Assigns = if Upd == [] -> ""; true ->
		      "\n\t" ++ string:join(utils:build_update(Upd, PIndex, NewBound), ";\n\t") ++ ";"
	      end,
    PAware = if Aware == [] -> ""; true -> string:join(Aware," and ") ++ " and " end,
    Cond = utils:build_pc_guard({Parent, {Pcname, PIndex, Value}}),
    ScheduleCon = if Schedule =/= [] ->
			 " and " ++  Pcname ++ "[" ++ integer_to_list(Schedule) ++ "] /= 1";
		     true ->
			  ""
		  end,

    %% Transfer previous bound variables to this children
    %% Transfer goes before assigments
    Transfer = if Parent == {} ->
		       "";
		  true ->
		       {_,ParentIndex,_} = Parent,
		       "-- Transfer bound variables -- \n\tbound[$1][" ++ integer_to_list(PIndex) ++ "] := bound[$1][" ++ integer_to_list(ParentIndex) ++ "];\n\t"
	       end,
    Trans = "\nSYS.$2.s0 -> $2.s0 {\n",
    RcvP = utils:build_rpred(Pred, PIndex, Bound, Msg, Parent),
    AllowedRcv = "tgt[$1] = 1 and ",
    Guards = "\tbroadcast(tgt,msg,j)[" ++ AllowedRcv ++ PAware ++ RcvP ++ " and " ++ Cond ++ ScheduleCon ++ "]/\n\t",
    Pc = Pcname ++ "[" ++ integer_to_list(PIndex) ++ "]",

    Exit = if ExitPoint == -1 -> Cnt + 1;  % no recursion, use 1 counter value
	      true  ->  ExitPoint
	   end,

    %% add bound assignments
    %% storing variables appearing in the received messages,
    %% if a variable appear many times in the same process, the latest is stored

    Reset = if Nil > 0 orelse ExitPoint > 0 -> "\n\tbound[$1][" ++ integer_to_list(PIndex) ++ "] := [];"; true -> "" end,
    Reset2 = if Nil > 0 -> "\n\t" ++ Pc ++ " := 0;"; true -> "" end,
    ReceivedSingnal = "\n\tOUT.received($2,msg);\n\t",
    Actions = ReceivedSingnal ++ Pc ++ " = " ++ integer_to_list(Exit) ++ ";" ++ Reset ++ Reset2,
    Signal = " \n ----- Receive ----- ",
    Print = [Signal,Trans,Guards, Transfer, BoundAssignments, Assigns, Actions,"\n}\n"],
    ets:insert(ProcessState, {state, Map#{v_name => NewBound, cnt => if ExitPoint  == - 1 -> Cnt + 1; true -> Cnt end}}),
%    file:write_file("foo.umc", Print, [append]),
    output_umc(Print),
 %   io:format("translate input ~p ~p ~p ~p ~n",[Pred, Vars, Exit, Cnt]),
    Exit.

preprocessing(CompDefs, Comp_inits) ->
    ets:insert(abcsystem, {attributes, #{}}),
    %% component indexes
    I = lists:seq(0,length(CompDefs) - 1),
    Comps = lists:zip(I, CompDefs),
    %% storing components behaviour definitions and initializing them into each own table
    lists:map(fun({I1, {comp, CName, [{attrs, AttList}, {obsrs, ObsList}, {beh, Behaviour, Init}]}}) -> % FULL OF SIDE-EFFECT
		      %% each component has an information table
		      ets:new(CName, [named_table]),
		      %% store the init behaviour of component definition CName
		      ets:insert(CName,Init),
		      ets:insert(CName, {obsrs, ObsList}),
		      %% store process definitions of this component
		      Fun = fun({def, ProcName, Args, Code}) ->
				    ets:insert(CName, {ProcName, {proc, Args, Code}});
			       ({def, ProcName, Args, Code, Times}) ->
				    ets:insert(CName, {ProcName, {proc, ProcName, Args, Code, Times}})
			    end,
		      lists:map(Fun, Behaviour),
		      %% Create the attributes list from component definitions
		      %% Assigning component indexes to each attribute name
		      Acc = ets:lookup_element(abcsystem, attributes, 2),
		      Map = lists:foldl(fun(X, M) ->
						case maps:is_key(X, M) of
						    true -> List = maps:get(X, M),
							    maps:put(X, [I1 | List], M);
						    false -> maps:put(X, [I1], M)
						end
					end,
					Acc, AttList),
		      ets:insert(abcsystem, {attributes, Map})
	      end, Comps),

    %% Collect attribute environments of all compnents
    Data = lists:foldl(fun({comp_init, CInit, {comp, _, Decl} = Body}, Acc) ->
			       ets:insert(abcsystem, {CInit, Body}),
			       [Decl | Acc]
		       end, [], Comp_inits),

    put(token,#{}), % for storing Tokens
    %% keep initialization data for attribute environments to output to the UMC model
    ets:insert(abcsystem, {data, Data}).

finish(CName_inits, []) -> % there's no SYS ::= declaration
    tail(CName_inits);
finish(_, [SYS]) -> % there is SYS ::=
    tail(SYS).

tail(SYS) ->
    Index = lists:seq(0,length(SYS)-1),
    S1 = lists:zip(Index, SYS),
    tail(S1,[],[],[],[]).

tail([], PcList, _ParList, Observables, TokenComps) ->
    Finished = "end System;\n\n",
    LineSep = io_lib:nl(),
    Pc = build_pc_list(PcList),
    Bound = build_bound_list(PcList),
   % Pchoice = build_choice_list(length(PcList)),
    %print(ParList),
    Data = ets:lookup_element(abcsystem, data, 2),
%    io:format("sys data ~p ~n",[Data]),

    Temp1 = [proplists:get_keys(X) || X <- Data],

    %% Get union of the attribute set
    Atts = sets:to_list(sets:from_list(lists:umerge(Temp1))),

    %% Build the attribute environment declaration
    Decl = lists:foldr(fun(X,Sum) ->
			Array = lists:foldl(fun(Comp, Acc) ->
						     case proplists:get_value(X, Comp) of
							 undefined ->
							     ["[]" | Acc];
							 Val ->
							     Val2 = data_eval(Val),
							     [Val2 | Acc]
						     end
					     end, [], Data),
			      [X ++ " -> [" ++ string:join(Array, ",") ++ "]" | Sum]
		end, [], Atts),
    Token = get(token),
    TokenDef = if Token =/= #{} ->
		       string:join(maps:keys(Token), ", ") ++ " : Token; \n\n";
		  true -> ""
	       end,
    TokenCompDef = string:join(lists:reverse(TokenComps), ",") ++ " : Token; \n\n",
    Object = "OO : System (" ++ string:join(Decl, ", ") ++ ");",

    ObserStates = string:join(lists:reverse(Observables), "\n"),
    Abstraction = "Abstractions {\n\tAction sending($1,$2) -> send($1,$2)\n\tAction received($1,$2) -> receive($1,$2)\n"
	++ ObserStates ++ "\n}\n\n",
    Print = [Finished, LineSep, TokenDef, TokenCompDef, Object, LineSep, Abstraction],
    output_umc(Print), %%FINISH
    FirstTrans ="\nTransitions:\ninit -> SYS {-/ \n\t" ++
	"pc := " ++ Pc ++ ";\n\t"
	"bound := " ++ Bound ++ ";\n\t"
	"for i in 0..pc.length-1 {\n\t\tself.allowsend(i);\n\t}}",
    Header = ets:lookup_element(umcsystem,header,2),
    UMCCODE = lists:reverse(ets:lookup_element(umcsystem,body,2)),
%    io:format("====code accumulated so far==== \n ~p",[UMCCODE]),
    file:write_file("foo.umc", [Header,FirstTrans,UMCCODE], [append]);
%    file:write_file("foo.umc", Print, [append]);

tail([{Index, {comp_init, Name, _}} | Rest], PcList, ParList, Obsrs, AnotherTokens) ->
    {comp, CName, _}  = ets:lookup_element(abcsystem, Name, 2),
    Interfaces = ets:lookup_element(CName, obsrs, 2),
    Observed = build_observable_states({Index, Name}, Interfaces),
    I = length(PcList),
    M4Code = atom_to_list(CName) ++ "(" ++ integer_to_list(I) ++ "," ++ atom_to_list(Name) ++ ")\n",
    Print = "---------- COMPONENT " ++ atom_to_list(Name) ++ " ------------ \n" ++ M4Code,
%    file:write_file("foo.umc", [Print ++ M4Code], [append]),
    output_umc(Print),
    Pc = ets:lookup_element(CName, num_procs, 2), % bc Pindex starts from 0
    ParvectorName = atom_to_list(CName) ++ "par_vector",
    ParVector = get(ParvectorName),
    %% print(ParVector),
    tail(Rest, [Pc | PcList], [ParVector | ParList], [Observed | Obsrs], [atom_to_list(Name) | AnotherTokens]).

build_observable_states(_, []) -> [];
build_observable_states({I, Name}, Data) ->
    F = fun(Att) ->
		"\tState OO." ++  Att ++ "[" ++ integer_to_list(I) ++ "] = $2 -> has_"++ Att ++ "(" ++ atom_to_list(Name) ++ ",$2)"
	end,
    L = [F(X) || X <- Data],
    string:join(L, "\n").

build_bound_list(L) ->
    build_bound_list(L,[]).

build_bound_list([],L) ->
    "[" ++ string:join(L,",") ++ "]";
build_bound_list([H|T],L) ->
    H1 = lists:seq(0,H),
    S = ["[]" || _ <- H1],
    String = "[" ++ string:join(S, ",") ++ "]",
    build_bound_list(T,[String | L]).


build_pc_list(L) ->
    build_pc_list(L,[]).

build_pc_list([],L) ->
    "[" ++ string:join(L,",") ++ "]";
build_pc_list([H|T],L) ->
    print("NUM PROCS: "),
    print(H + 1),
    H1 = lists:seq(0,H),
    S = ["1" || _ <- H1],
    String = "[" ++ string:join(S, ",") ++ "]",
    build_pc_list(T,[String | L]).

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
data_eval(empty_set) -> % set as vector for now
    data_eval(empty_vector);
% an array of single elements
data_eval({bracket2, List}) -> % set as vector for now
    data_eval({bracket, List});
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
bound_assignment([], Vars, Index) ->
%    io:format("previous vars empty ~n",[]),
    Index1 = lists:seq(0, length(Vars) - 1),
    Msg = lists:zip(Vars, Index1),
    B1 = string:join(["bound[$1][" ++ Index ++ "][" ++ integer_to_list(proplists:get_value(X,Msg)) ++ "] = msg[" ++ integer_to_list(proplists:get_value(X,Msg)) ++ "];" || X <- Vars], "\n\t"),
    [B1, Msg, Msg];
bound_assignment(PreviousVars, Vars, Index) ->
    Prev1 = [X || {X,_} <- PreviousVars], % previous variables
   % io:format("previous vars ~p~n",[PreviousVars]),
    Added = Vars -- Prev1,  % added vars
  %  io:format("added vars ~p~n",[Added]),
    Length1 = length(PreviousVars),
    Index1 = lists:seq(0, length(Vars) - 1),
    Msg = lists:zip(Vars, Index1),
    Index2 = lists:seq(Length1, Length1 + length(Added) - 1),
    New = lists:zip(Added, Index2),
    New2 = PreviousVars ++ New,
%%    L = [proplists:get_value(X, PreviousVars) || X <- Vars, proplists:get_value(X) =/= undefined],
    B1 = string:join(["bound[$1][" ++ Index ++ "][" ++ integer_to_list(proplists:get_value(X,New2)) ++ "] = msg[" ++ integer_to_list(proplists:get_value(X,Msg)) ++ "];" || X <- Vars], "\n\t"),
   % io:format("boundassing ~p newbound ~p~n",[B1, New2]),
    [B1, New2, Msg].


output_umc(Code) ->
    Body = ets:lookup_element(umcsystem,body,2),
    ets:insert(umcsystem, {body, [Code|Body]}).
