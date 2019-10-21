-module(cust).
-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).
-export([init_beh/2]).
init_beh(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> f(_C,_V) end,
	fun(_V) -> a(_C,_V) end
    ]).

f(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> att(send,_LclE) == true end,
	{fun(_LclE) -> acms end,fun(_LclE) -> att(cid,_LclE) end,fun(_LclE) -> att(loc,_LclE) end,fun(_LclE) -> att(day,_LclE) end,fun(_LclE) -> att(price,_LclE) end},
	fun(_LclE, _RmtE) -> att(stype,_RmtE) == broker end,
	[{send, fun(_LclE) -> false end}]},
	fun(_V) -> f(_C,[]) end}
	).

a(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == offer andalso att(price,_LclE) >= msg(4,_M) andalso (user_code:distance(att(loc,_LclE),msg(3,_M)) =< att(dist,_LclE)) end,
	{x,h,l,p,b},
	[{price, fun(_LclE,_M) -> msg(4,_M) end},
	{favh, fun(_LclE,_M) -> msg(2,_M) end},
	{ref, fun(_LclE,_M) -> msg(5,_M) end}]},
	fun(_V) -> a(_C,[]) end},
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == finish end,
	{x,z},
	[]},
	fun(_V) -> b(_C,[]) end}
    ]).

b(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> att(favh,_LclE) =/= undef end,
	{fun(_LclE) -> book end,fun(_LclE) -> att(cid,_LclE) end,fun(_LclE) -> att(day,_LclE) end,fun(_LclE) -> att(ref,_LclE) end},
	fun(_LclE, _RmtE) -> att(hid,_RmtE) == att(favh,_LclE) end,
	[]},
	fun(_V) -> '_fun1'(_C,_V) end},
	{{fun(_LclE) -> att(favh,_LclE) == undef end,
	{},
	fun(_LclE, _RmtE) -> false end,
	[{send, fun(_LclE) -> true end}]},
	fun(_V) -> a(_C,[]) end}
    ]).

'_fun1'(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == confirm end,
	{x},
	[]},
	nil},
	{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == toolate end,
	{x},
	[{send, fun(_LclE,_M) -> true end}]},
	fun(_V) -> a(_C,[]) end}
    ]).

