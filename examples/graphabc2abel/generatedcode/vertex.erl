-module(vertex).
-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).
-export([init_beh/2]).
init_beh(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> f(_C,_V) end,
	fun(_V) -> d(_C,_V) end,
	fun(_V) -> a(_C,_V) end,
	fun(_V) -> t(_C,_V) end
    ]).

f(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> att(send,_LclE) == 1 andalso att(assigned,_LclE) == 0 end,
	{fun(_LclE) -> 'try' end,fun(_LclE) -> user_code:min_colour(att(used,_LclE)) end,fun(_LclE) -> att(round,_LclE) end},
	fun(_LclE, _RmtE) -> sets:is_element(att(id,_LclE),att(nbr,_RmtE)) end,
	[{send, fun(_LclE) -> 0 end},
	{colour, fun(_LclE) -> user_code:min_colour(att(used,_LclE)) end}]},
	fun(_V) -> f(_C,[]) end}
	).

a(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> (sets:size(att(nbr,_LclE)) == att(counter,_LclE) + att(done,_LclE)) andalso att(colour,_LclE) > 0 andalso (not sets:is_element(att(colour,_LclE),sets:union(att(constraints,_LclE),att(used,_LclE)))) end,
	{fun(_LclE) -> donec end,fun(_LclE) -> att(colour,_LclE) end,fun(_LclE) -> att(round,_LclE) + 1 end},
	fun(_LclE, _RmtE) -> sets:is_element(att(id,_LclE),att(nbr,_RmtE)) end,
	[{assigned, fun(_LclE) -> 1 end}]},
	nil}
	).

t(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == 'try' andalso att(id,_LclE) > att(id,_RmtE) andalso att(round,_LclE) == msg(3,_M) end,
	{x,y,z},
	[{counter, fun(_LclE,_M) -> att(counter,_LclE) + 1 end}]},
	fun(_V) -> t(_C,[]) end},
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == 'try' andalso att(id,_LclE) < att(id,_RmtE) andalso att(round,_LclE) == msg(3,_M) end,
	{x,y,z},
	[{counter, fun(_LclE,_M) -> att(counter,_LclE) + 1 end},
	{constraints, fun(_LclE,_M) -> sets:union(att(constraints,_LclE),sets:from_list([msg(2,_M)])) end}]},
	fun(_V) -> t(_C,[]) end},
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == 'try' andalso att(id,_LclE) > att(id,_RmtE) andalso att(round,_LclE) < msg(3,_M) end,
	{x,y,z},
	[{round, fun(_LclE,_M) -> msg(3,_M) end},
	{send, fun(_LclE,_M) -> 1 end},
	{counter, fun(_LclE,_M) -> 1 end},
	{constraints, fun(_LclE,_M) -> sets:new() end}]},
	fun(_V) -> t(_C,[]) end},
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == 'try' andalso att(id,_LclE) < att(id,_RmtE) andalso att(round,_LclE) < msg(3,_M) end,
	{x,y,z},
	[{round, fun(_LclE,_M) -> msg(3,_M) end},
	{send, fun(_LclE,_M) -> 1 end},
	{counter, fun(_LclE,_M) -> 1 end},
	{constraints, fun(_LclE,_M) -> sets:from_list([msg(2,_M)]) end}]},
	fun(_V) -> t(_C,[]) end}
    ]).

d(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == donec andalso att(round,_LclE) >= msg(3,_M) end,
	{x,y,z},
	[{done, fun(_LclE,_M) -> att(done,_LclE) + 1 end},
	{used, fun(_LclE,_M) -> sets:union(att(used,_LclE),sets:from_list([msg(2,_M)])) end}]},
	fun(_V) -> d(_C,[]) end},
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == donec andalso att(round,_LclE) < msg(3,_M) end,
	{x,y,z},
	[{round, fun(_LclE,_M) -> msg(3,_M) end},
	{done, fun(_LclE,_M) -> att(done,_LclE) + 1 end},
	{send, fun(_LclE,_M) -> 1 end},
	{counter, fun(_LclE,_M) -> 0 end},
	{used, fun(_LclE,_M) -> sets:union(att(used,_LclE),sets:from_list([msg(2,_M)])) end},
	{constraints, fun(_LclE,_M) -> sets:new() end}]},
	fun(_V) -> d(_C,[]) end}
    ]).

