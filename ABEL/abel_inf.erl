%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2018, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2018 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(abel_inf).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 request_id/1,
	 connect/2,
	 register/1,
	 unregister/2,
	 request_parent/1,
	 send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {id,
		addr,
		db, % AbC components connected
		parent, % unique parent
		nid, % next data msg to be received
		children, % set of childrens server pid
		counter,
		queue,
		msgset % queue of data message
	       }).

%%%===================================================================
%%% API
%%%===================================================================
connect(Pid, List) ->
    gen_server:call(Pid, {connect, List}).

request_parent(Pid) ->
    gen_server:call(Pid, request_parent).

request_id(Pid) ->
    gen_server:call(Pid, request_id).

send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).

register(Pid) ->
    gen_server:call(Pid, register).

unregister(Pid, Agent) ->
    gen_server:cast(Pid, {unregister,Agent}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Id]) ->
    {ok, #state{db = ets:new(db,[]),
		id = Id,
		nid = 0,
		addr = self(),
		counter = 0,
		queue = gb_trees:empty()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% ogranizing nodes
handle_call({tree, List}, _From, #state{id = Id} = State) ->
    Parent = extract_parent_tree(Id, List),
    Children = extract_children_tree(Id, List),
    {reply, ok, State#state{parent = Parent, children = Children}};
handle_call({star, List}, _From, #state{id = Id} = State) ->
    Parent = extract_parent_star(Id, List),
    Children = extract_children_star(Id, List),
    {reply, ok, State#state{parent = Parent, children = Children}};
handle_call(register, {Pid, _}, #state{db = Table} = State) ->
    ets:insert(Table, {Pid}),
    {reply, ok, State};

handle_call(request_id, _From, #state{parent = [], counter = Counter} = State) ->
    {reply, Counter, State#state{counter = Counter + 1}};
handle_call(request_id, From, #state{addr = Addr, parent = Pid} = State) ->
    gen_server:cast(Pid, {request_id, [Addr, From]}),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({unregister, Pid}, #state{db = Table} = State) ->
    %io:format("Serving node ~p unregisters agent'scomponent ~p ~n",[Addr, Pid]),
    ets:delete(Table, Pid),
    {noreply, State};
handle_cast({request_id, [From | T]}, #state{parent = [], counter = Counter} = State) ->
    gen_server:cast(From, {reply_id, Counter, T}),
    {noreply, State#state{counter = Counter + 1}};
handle_cast({request_id, From}, #state{addr = Addr, parent = Parent} = State) ->
    gen_server:cast(Parent, {request_id, [Addr | From]}),
    {noreply, State};
handle_cast({reply_id, Counter, [From]}, State) ->
    gen_server:reply(From, Counter),
    {noreply, State};
handle_cast({reply_id, Counter, [From | T]}, State) ->
    gen_server:cast(From, {reply_id, Counter, T}),
    {noreply, State};

handle_cast({send, {Id, Msg, Sender}}, #state{db = Table,
					      nid = Nid,
					      queue = Queue,
					      children = Children,
					      addr = Addr,
					      parent = Parent} = State) ->
    NewQueue = gb_trees:insert(Id, {Msg, Sender}, Queue),
    {TopId, {TopMsg, TopSender}, Rest} = gb_trees:take_smallest(NewQueue),
    if TopId == Nid ->
	    List = ets:tab2list(Table),
	    send_to_comp(List, {TopId, TopMsg}, TopSender),
	    send_to_children(Children -- [TopSender], {TopId,TopMsg}, Addr),
	    send_to_parent(if Parent == TopSender -> []; true -> Parent end, {TopId,TopMsg}, Addr),
	    {noreply, State#state{nid = Nid + 1, queue = Rest}};
       true ->
	    io:format("~p did not forward a message (~p, ~p, ~p) where Nid = ~p~n",[Addr, TopId, TopMsg, TopSender, Nid]),
	    {noreply, State#state{queue = NewQueue}}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
extract_parent_tree(1, _) -> [];
extract_parent_tree(Id, List) ->
    proplists:get_value(Id div 2, List).


extract_children_tree(Id, List) when 2* Id > length(List) -> [];
extract_children_tree(Id, List) when 2* Id == length(List) ->
    [proplists:get_value(Id * 2, List)];
extract_children_tree(Id, List) ->
    [proplists:get_value(Id * 2, List), proplists:get_value(Id * 2 + 1, List)].


extract_parent_star(1, _) -> [];
extract_parent_star(_, List) ->
    proplists:get_value(1, List).

extract_children_star(1, List) -> [Add || {Id,Add} <- List, Id > 1];
extract_children_star(_, _) ->  [].

send_to_comp([], _, _) -> ok;
send_to_comp([{Pid} | T], Msg, Sender) ->
    if Sender =/= Pid ->
	    gen_statem:cast(Pid, {data, Msg});
       true ->
	    ok
    end,
    send_to_comp(T,Msg,Sender).

send_to_children([], _, _) ->
    ok;
send_to_children([Pid |T], {Id, Msg}, Sender) ->
    gen_server:cast(Pid, {send, {Id, Msg, Sender}}),
    send_to_children(T, {Id, Msg}, Sender).

send_to_parent([], _, _) ->
    ok;
send_to_parent(Parent, {Id, Msg}, Sender) ->
    gen_server:cast(Parent, {send, {Id, Msg, Sender}}).
