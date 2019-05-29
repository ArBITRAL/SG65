%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2018, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2018 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(abel_coord).

-behaviour(gen_statem).

%% API
-export([new_component/3,start_component/1,start_component/2]).

-export([prefix/2,choice/2,par/2]).

-export([dispatch/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-record(data, {
	  queue,
	  agent,
	  procs,
	  interface,
	  status,   % storing the status of each process, send or receive at the moment
	  init_beh, % initial behaviour
	  mid,
	  counter,
	  module
	 }).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
new_component(M, Env, I) ->
    gen_statem:start_link(?MODULE, [M, Env, I], []).

start_component(Pid) ->
    gen_statem:cast(Pid, {start_beh, init_beh}).

start_component(Pid, Init) ->
    gen_statem:cast(Pid, {start_beh, Init}).

%% to be called by processes
prefix(Pid, {Act, Con}) ->
    gen_statem:call(Pid, {Act, Con}).

choice(Pid, BehList) ->
    gen_statem:call(Pid, {choice, BehList}).

par(Pid, BehList) ->
    gen_statem:cast(Pid, {parallel, BehList}).

%% call(Pid, F) ->
%%     gen_statem:cast(Pid, {call, F}).

%% to be called by infrastructure
dispatch(Pid, Msg) ->
    gen_statem:cast(Pid, {dispatch, Msg}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
		  gen_statem:init_result(term()).
init([M,Env,I]) ->
    process_flag(trap_exit, true),
    {ok, Server} = abel_reg:reg(), %% register to the inf
%    InitBeh = erlang:apply(M,init_beh,[self(),[]]),
    {ok, {Env, false}, #data{queue = gb_trees:empty(),
			     status = #{}, agent = Server, procs = 0, module = M,
			     interface = tuple_to_list(I), mid = -1, counter = 0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for every event a gen_statem receives.
%% @end
%%--------------------------------------------------------------------
-spec handle_event('enter',
		   OldState :: term(),
		   State :: term(),
		   Data :: term()) ->
			  gen_statem:state_enter_result(term());
		  (gen_statem:event_type(),
		   Msg :: term(),
		   State :: term(),
		   Data :: term()) ->
			  gen_statem:event_handler_result(term()).
%% starting point of the execution
handle_event(cast, {start_beh, F}, _State, Data = #data{module = M, procs = NumProcs}) ->
    erlang:apply(M,F,[self(),[]]),
    %Pid = spawn_link(F),
    %% io:format("create 1 proc ~p ~n",[Pid]),
    {keep_state, Data};
%, Data#data{procs = NumProcs + length(Pids)}};

%% handle request of parallel process creation
handle_event(cast, {parallel, FunList}, _State, Data  = #data{module = M, procs = NumProcs}) ->
    Pids = [spawn_link(F) || F <- FunList],
%    io:format("create ~p processes ~n",[length(Pids)]),
    {keep_state, Data#data{procs = NumProcs + length(Pids)}};

% a process terminated, remove from the Status list
handle_event(info, {'EXIT', Pid, normal}, {Env,_}, Data = #data{agent = Agent, status = Status, procs = NumProcs, queue = Queue}) ->
    NewStatus = maps:remove(Pid, Status),

    %%% HARD CODE FOR APPLICATION %%%
    %%% REPORT OUTCOME %%%
    %% stable matching
    case maps:is_key(partner,Env) andalso maps:is_key(gender,Env) andalso maps:get(partner,Env) =/= 0 andalso maps:get(gender,Env) == 0 of
    	false -> ok;
    	_ ->
	    %% we can't stop the component
    	    global:whereis_name(matchsys) ! {maps:get(id,Env), maps:get(partner,Env)}
    end,
    %% graphcolouring
    case maps:is_key(assigned,Env) andalso maps:get(assigned,Env) == 1 of
    	true ->
	    %% we can stop the component
	    {stop, normal, {maps:get(report, Env), maps:get(id,Env), maps:get(colour,Env), maps:get(round, Env)}};
    	false ->
	    case NumProcs - 1 == maps:size(NewStatus) andalso not gb_trees:is_empty(Queue) of
		true -> %% last send can trigger dispatching a pending message
		    {keep_state, Data#data{status = NewStatus, procs = NumProcs - 1},
		     [{next_event, internal, dispatch}]};
		false ->
		    {keep_state, Data#data{status = NewStatus, procs = NumProcs - 1}}
	    end
    end;

%% handle a sending operation of any process, guarded by G, update U followed
handle_event({call, {Pid, _} = From},
	     {{send, G, Msg, Pred, U}, Con},
	     {Env, Sending},
	     Data = #data{agent = Agent, status = Status, procs = NumProcs, queue = Queue, interface = I, mid = Mid, counter = Counter, module = M}) ->
    %% register this sending action in the status as discarding any message
    NewStatus = maps:update_with(Pid, fun(_) -> discarding end, discarding, Status),

    case G(Env) of
	true ->
	    FreshId =  if Mid == -1 ->
			       abel_inf:request_id(Agent);
			  true -> Mid end,
     %% io:format("===~p gonna send msg ~p, color = ~p ~n",
     %% 	      [maps:get(id,Env), Msg, maps:get(colour,Env)]),
	    case FreshId == Counter of
		true ->
		    PublicEnv = maps:with(I,Env),
		    NewPred = partial_eval(Pred, PublicEnv),
		    NewMsg = partial_eval(Msg, Env),

		    abel_inf:send(Agent, {FreshId, {NewPred, NewMsg, PublicEnv}, self()}),
		    %% Applying update
		    NewEnv = lists:foldl(fun({X,V},Sum) ->
						 EvalV =
						     case is_function(V) of
							 true -> V(Env);
							 false -> V
						     end,
						 maps:update(X, EvalV, Sum)
					 end, Env, U),
		    %% io:format("===~p sent msg ~p, color = ~p ~n",
	      	    %%   [maps:get(id,NewEnv), NewMsg, maps:get(colour,NewEnv)]),
	    %% case maps:is_key(gender,Env) andalso maps:get(gender,NewEnv) of
	    %% 	1 ->
	    %% 	    io:format("===~p sent msg ~p, partner = ~p ~n",
	    %%  	      [maps:get(id,NewEnv), NewMsg, maps:get(partner,NewEnv)]);
	    %% 	0 ->
	    %% 	    io:format("===~p sent msg ~p, partner = ~p, cm1 ~p , cm2 ~p , lock ~p ~n",
	    %%  	      [maps:get(id,NewEnv), NewMsg, maps:get(partner,NewEnv),
	    %% 	      maps:get(cm1,NewEnv),maps:get(cm2,NewEnv),maps:get(lock,NewEnv)])
	    %% end,
		    gen_statem:reply(From, {Con,[]}),
		    Status2 = maps:remove(Pid, Status),
		    case NumProcs == maps:size(NewStatus) andalso not gb_trees:is_empty(Queue) of %% the second condition is to save some computation power due to doing dispatching without any message yet
		    	true -> %% last send can trigger dispatching a pending message
		    	    {next_state, {NewEnv, Sending}, Data#data{status = Status2, mid = -1, counter = Counter + 1},
		    	     [{next_event, internal, dispatch}]};
		    	false -> %% otherwise
			    {next_state, {NewEnv, Sending}, Data#data{status = Status2, mid = -1, counter = Counter + 1}}
			end;
		false ->
		    case NumProcs == maps:size(NewStatus) andalso not gb_trees:is_empty(Queue) of
			true -> %% last send can trigger dispatching a pending message
			    {keep_state, Data#data{status = NewStatus, mid = FreshId},
			     [postpone,{next_event, internal, dispatch}]};
			false ->
			    {keep_state, Data#data{status = NewStatus, mid = FreshId},
			     [postpone]}
		    end
	    end;
	false ->
	    case NumProcs == maps:size(NewStatus) andalso not gb_trees:is_empty(Queue) of
		true -> %% last send can trigger dispatching a pending message
		    {keep_state, Data#data{status = NewStatus}, [postpone,{next_event, internal, dispatch}]};
		false ->
		    {keep_state, Data#data{status = NewStatus}, [postpone]}
	    end
    end;

%% no mixed choice of sending and receiving
%% BehList = [{Description},...]
%% SendDescription = {send, Guard, Msg, Pred, U}
%% RecvDescription = {recv, Guard, Pred, U}
handle_event({call, {Pid,_} = From}, {choice, BehList}, {Env, Sending}, Data = #data{procs = NumProcs, status = Status, agent = Agent, queue = Queue, interface = I, mid = Mid, counter = Counter, module = M}) ->
    % pick any send
    case lists:any(fun({X,_}) ->
			   element(1,X) == send
		   end,
		   BehList) of
        true ->
	    NewStatus = maps:update_with(Pid, fun(_) -> discarding end, discarding,Status),
	    case lists:dropwhile(fun({X,_}) ->
	    				 G = element(2,X), % the second element of X is a guard
	    				 not (G(Env))
	    			 end,
	    			 BehList) of
	    	[] ->
	    	    %% no sending is possible because of guards, postpone this, possbily trigger message dispatching if this complement full status
	    	    case NumProcs == maps:size(NewStatus) andalso not gb_trees:is_empty(Queue) of
			true -> %% last send can trigger dispatching a pending message
	    		    {keep_state, Data#data{status = NewStatus}, [postpone,{next_event, internal, dispatch}]};
	    		false ->
	    		    {keep_state, Data#data{status = NewStatus}, [postpone]}
		    end;

	    	[{{_, _, {}, _, Update}, Con} | _] ->   %% pick the first sending action, followed by an update, dealing with empty send
		    NewEnv = lists:foldl(fun({X,V},Sum) ->
						 EvalV =
						     case is_function(V) of
							 true -> V(Env);
							 false -> V
						     end,
						 maps:update(X, EvalV, Sum)
					 end, Env, Update),
		    gen_statem:reply(From, {Con,[]}),
		    Status2 = maps:remove(Pid,Status),
		    case maps:size(NewStatus) == NumProcs andalso not gb_trees:is_empty(Queue) of
		    	true ->
		    	    {next_state, {NewEnv, Sending}, Data#data{status = Status2},
		    	     [{next_event, internal, dispatch}]};
		    	false ->
		    	    {next_state, {NewEnv, Sending}, Data#data{status = Status2}}
		    end;
	    	[{{_, _, Msg, Pred, Update}, Con} | _] ->   %% pick the first sending action, followed by an update
	    	    FreshId =  if Mid == -1 ->
				       abel_inf:request_id(Agent);
				  true -> Mid end,

	    	    case FreshId == Counter of
	    		true ->
			    PublicEnv = maps:with(I,Env),
	    		    NewPred = partial_eval(Pred, PublicEnv),
	    		    NewMsg = partial_eval(Msg, Env),

			    abel_inf:send(Agent, {FreshId, {NewPred, NewMsg, PublicEnv}, self()}),
	    		    NewEnv = lists:foldl(fun({X,V},Sum) ->
	    						 EvalV =
	    						     case is_function(V) of
	    							 true -> V(Env);
	    							 false -> V
	    						     end,
	    						 maps:update(X, EvalV, Sum)
	    					 end, Env, Update),
	    		    gen_statem:reply(From, {Con,[]}),
	    		    Status2 = maps:remove(Pid,Status),
	    		    case maps:size(NewStatus) == NumProcs andalso not gb_trees:is_empty(Queue) of
	    		    	true ->
	    		    	    {next_state, {NewEnv, Sending}, Data#data{status = Status2, mid = -1, counter = Counter + 1},
	    		    	     [{next_event, internal, dispatch}]};
	    		    	%% last send can trigger discarding a message
	    		    	false ->
	    			    {next_state, {NewEnv, Sending}, Data#data{status = Status2, mid = -1, counter = Counter + 1}}
			    end;
	    		false ->
	    		    case maps:size(NewStatus) == NumProcs andalso not gb_trees:is_empty(Queue) of
	    		    	true ->
	    		    	    {keep_state, Data#data{status = NewStatus, mid = FreshId},
	    		    	     [postpone,{next_event, internal, dispatch}]};
	    		    	%% last send can trigger discarding a message
	    		    	false ->
	    		    	    {keep_state, Data#data{status = NewStatus, mid = FreshId},
				    [postpone]}
	    		    end
		    end
	    end;
	false ->
	    % this is a choice with receiving, after register it for receiving, handling similar to ordinary receive
	    RecvList = [{G, RPred, X, U,Con} || {{recv, G, RPred, X, U}, Con} <- BehList], %% [{{recv, G, P, U},C}, ...]
	    NewStatus = maps:update_with(Pid, fun(_) -> {From, RecvList} end, {From, RecvList}, Status),
	    case not gb_trees:is_empty(Queue) andalso NumProcs == maps:size(NewStatus) of
	  	true ->
	  	    {keep_state, Data#data{status = NewStatus},
	  	     [{next_event, internal, dispatch}]};
	  	false ->
	  	    {keep_state, Data#data{status = NewStatus}}
	    end
    end;

handle_event({call, {Pid,_} = From}, {{recv, G, RPred, X, U},Con}, _State, Data = #data{status = Status, queue = Queue, procs = NumProcs}) ->
    NewStatus = maps:update_with(Pid, fun(_) -> {From, {G, RPred, X, U, Con}} end, {From, {G, RPred, X, U, Con}}, Status),
    case not gb_trees:is_empty(Queue) andalso NumProcs == maps:size(NewStatus) of
	true ->
	    {keep_state, Data#data{status = NewStatus}, [{next_event, internal, dispatch}]};
	false ->
	    {keep_state, Data#data{status = NewStatus}}
    end;

handle_event(cast, {data, {Id, Msg}}, {Env,_},
	     Data = #data{queue = Queue, status = Status, procs = NumProcs}) ->
    NewQueue = gb_trees:insert(Id, Msg, Queue),  % does it really need to add to the queue?
    case NumProcs == maps:size(Status) andalso NumProcs > 0 of
	true ->
%	    io:format("msg forwarded, hanlde ~p ~n",[NumProcs]),
	    {keep_state, Data#data{queue = NewQueue},
	     [{next_event, internal, dispatch}]};
	false ->
%	    io:format("msg forwarded, no ~p ~n",[NumProcs]),
	    {keep_state, Data#data{queue = NewQueue}}
    end;

handle_event(internal, dispatch, {REnv, Sending}, Data = #data{queue = Queue, status = Status, mid = Mid, counter = Counter, module = M}) ->
    %% check if there any procs can receive this message
    {_, Msg, Rest} = gb_trees:take_smallest(Queue),
    Result = dispatch_internal(Msg, maps:to_list(Status), REnv),
    NewSending = if (Counter + 1 == Mid orelse Mid == -1) -> not Sending; true -> Sending end, %state change
    %% Statechange = Sending =/= NewSending,
    case Result == [] of
	true ->
	    case gb_trees:is_empty(Rest) of
	    	true ->
	    	    {next_state, {REnv, NewSending}, Data#data{queue = Rest, counter = Counter + 1}};
	    	false ->
	    	    handle_event(internal, dispatch, {REnv, NewSending}, Data#data{queue = Rest, counter = Counter + 1})
	    end;
	false ->
	    %%discard_message_and_reply
	    ReplyTo = {FromPid,_} = element(1,Result),
	    Update = element(2, Result),
	    MContent = element(2,Msg),
	    NewStatus = maps:remove(FromPid, Status),
	    Con = element(3,Result),
	    Vars = element(4,Result),
	    NewEnv = lists:foldl(fun({X,V},Sum) ->
					 EvalV =
					     case is_function(V) of
						 true -> V(REnv,MContent);
						 false -> V
					     end,
					 maps:update(X, EvalV, Sum)
				 end, REnv, Update),
	      %% io:format("===~p ACCEPT ~p of ~p, constraints ~p, used ~p, counter ~p, done ~p,  update local counter ~p ~n",
	      %% 	      [maps:get(id,NewEnv), MContent, maps:get(id,element(3,Msg)), sets:to_list(maps:get(constraints,NewEnv)), sets:to_list(maps:get(used,NewEnv)), maps:get(counter,NewEnv), maps:get(done,NewEnv), Counter + 1]),
	    %% case maps:is_key(gender,NewEnv) andalso maps:get(gender,NewEnv) of
	    %% 	1 ->
	    %% 	    io:format("===~p ACCEPT ~p of ~p, partner = ~p ~n",
	    %% 		      [maps:get(id,NewEnv), MContent, maps:get(id,element(3,Msg)), maps:get(partner,NewEnv)]);
	    %% 	0 ->
	    %% 	    io:format("===~p ACCEPT ~p of ~p, partner = ~p, cm1 ~p , cm2 ~p , lock ~p ~n",
	    %% 		      [maps:get(id,NewEnv), MContent, maps:get(id,element(3,Msg)), maps:get(partner,NewEnv),
	    %% 		       maps:get(cm1,NewEnv),maps:get(cm2,NewEnv),maps:get(lock,NewEnv)])
	    %% end,
	    gen_statem:reply(ReplyTo, {Con,lists:zip([Vars],[MContent])}),
	    {next_state, {NewEnv, NewSending}, Data#data{queue = Rest, status = NewStatus, counter = Counter + 1}}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
		       any().
terminate(normal, _State, {Report, A,B,C}) ->
    %% report - application specific - graph colouring
    Report ! {done, A,B,C},
    ok;
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
	OldVsn :: term() | {down,term()},
	State :: term(), Data :: term(), Extra :: term()) ->
			 {ok, NewState :: term(), NewData :: term()} |
			 (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
dispatch_internal(_, [], _) ->
    []; % no process can receive the message
dispatch_internal(Msg, [{_,discarding} | T], REnv) -> % not dispatching to sending process
    dispatch_internal(Msg, T, REnv);
dispatch_internal({SPred, MsgContent, SEnv} = Msg, [{_,{From, List}}|T], REnv) when is_list(List) -> % a set of receive actions registered by a choice process
    case lists:dropwhile(fun({G,RPred, X, _,_}) ->
				 Check = not (tuple_size(MsgContent) == tuple_size(X) andalso G(REnv) andalso RPred(REnv,MsgContent,SEnv) andalso SPred(REnv)),
				 Check
			 end,
			 List) of
	[] ->
	    dispatch_internal(Msg, T, REnv);
	[{_,_,X,U,Con} |_] ->
	   % io:format("GOES HERE ~n"),
	    {From, U, Con, X}
    end;
% ordinary receive
dispatch_internal({SPred, MsgContent, SEnv} = Msg, [{_, {From, {G, RPred, X, U, Con}}}|T], REnv) ->
    case tuple_size(X) == tuple_size(MsgContent) andalso G(REnv) andalso RPred(REnv, MsgContent, SEnv) andalso SPred(REnv) of
	true ->
	    {From, U, Con, X};
	false ->
	    dispatch_internal(Msg,T, REnv)
    end;
dispatch_internal(A,B,C) ->
    io:format("other case which should not be comp ~p: ~p ~p  ~n",[maps:get(id,C), A, B]).

partial_eval(Pred, Env) when is_function(Pred) ->
    F = fun(E1) ->
		fun(E2) ->
			Pred(E1,E2)

		end
	end,
    F(Env);
partial_eval(Msg, Env) when is_tuple(Msg) ->
    F = fun(X) ->
		if is_function(X) ->
			X(Env);
		   true ->
			X
		end
	end,
    list_to_tuple(lists:map(F, tuple_to_list(Msg))).
