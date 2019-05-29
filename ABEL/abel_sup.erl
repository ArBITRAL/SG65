%%%-------------------------------------------------------------------
%% @doc abel top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(abel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	start_link/1,
	 start_worker/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    start_link(1).

start_link(N) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [N]).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([N]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    PoolTable = ets:new(tree, [public, set]),
    true = ets:insert(PoolTable, {c, 0}),
    PoolTableCounter = ets:new(treecounter, [named_table,public, set]),
    true = ets:insert(PoolTableCounter, {c, 0}),
    CreateSpec = fun(X) ->
			 child_specs(X, {'abel_inf', start_link, [X]}, PoolTable)
		 end,
    Children = lists:map(CreateSpec, lists:seq(1,N)),

    Registration = #{ id => N+1,
		      start => {'abel_reg', start_link, [N, PoolTable]},
		      restart => permanent,
		      shutdown => 5000,
		      type => worker,
		      modules => ['abel_reg']},
    {ok, {SupFlags, [Registration | Children]}}.

%    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
child_specs(Id, {M,F,A}, PoolTable) ->
    {Id, {?MODULE, start_worker, [Id,{M,F,A},PoolTable]}, permanent, 2000, worker, [M]}.

start_worker(Id, {M,F,A},PoolTable) ->
    {ok, Pid} = erlang:apply(M,F,A),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.
