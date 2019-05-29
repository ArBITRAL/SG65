%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2018, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2018 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(abel_reg).

-behaviour(gen_server).

%% API
-export([start_link/2, reg/0, create_tree/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pool, size }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(N, PoolTable) ->
    gen_server:start_link({global,?SERVER}, ?MODULE, [N,PoolTable], []).


%% [1,2,3,4,5]
create_tree(binary) ->
    gen_server:call({global,?SERVER}, create_tree);
create_tree(star) ->
    gen_server:call({global,?SERVER}, create_star).


reg() ->
    Server = gen_server:call({global,?SERVER}, serving_node_pid),
    ok = abel_inf:register(Server),
    {ok, Server}.

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
init([N,PoolTable]) ->
    process_flag(trap_exit, true),
    {ok, #state{size = N, pool = PoolTable}}.

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
handle_call(create_tree, _From, #state{pool = PoolTable} = State) ->
    L = ets:tab2list(PoolTable),
    Nodes = lists:keydelete(c,1,L),
   % io:format("List of nodes ~p~n",[Tree]),
    [gen_server:call(Pid,{tree,Nodes}) || {_, Pid} <- Nodes],
    {reply, ok, State};
handle_call(create_star, _From, #state{pool = PoolTable} = State) ->
    L = ets:tab2list(PoolTable),
    Nodes = lists:keydelete(c,1,L),
   % io:format("List of nodes ~p~n",[Tree]),
    [gen_server:call(Pid,{star,Nodes}) || {_, Pid} <- Nodes],
    {reply, ok, State};
handle_call(serving_node_pid, _From, #state{pool = PoolTable, size = Size} = State) ->
    Reply =  try
		 N = ets:update_counter(PoolTable, c, {2, 1, Size, 1}),
		 [{N, Worker}] = ets:lookup(PoolTable, N),
		 Worker
	     catch
		 _:Error ->
		     {error, Error}
	     end,
    {reply, Reply, State}.


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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
