-module(hotel).
-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).
-export([init_beh/2]).
init_beh(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> p(_C,_V) end,
	fun(_V) -> b(_C,_V) end
    ]).

p(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == acms end,
	{x,c,d,b},
	[]},
	fun(_V) -> '_fun1'(_C,_V) end}
	).

h(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> user_code:selector1(att(room,_LclE),var(d,V)) > 0 end,
	{fun(_LclE) -> offer end,fun(_LclE) -> var(c,V) end,fun(_LclE) -> att(hid,_LclE) end,fun(_LclE) -> att(locality,_LclE) end,fun(_LclE) -> user_code:selector2(att(price,_LclE),var(d,V),var(b,V)) end},
	fun(_LclE, _RmtE) -> att(bid,_RmtE) == var(b,V) end,
	[]},
	nil},
	{{fun(_LclE) -> user_code:selector1(att(room,_LclE),var(d,V)) =< 0 end,
	{fun(_LclE) -> nooffer end,fun(_LclE) -> var(c,V) end},
	fun(_LclE, _RmtE) -> att(bid,_RmtE) == var(b,V) end,
	[]},
	nil}
    ]).

b(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == book end,
	{x,c,d,refc},
	[]},
	fun(_V) -> '_fun2'(_C,_V) end}
	).

c(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> user_code:selector1(att(room,_LclE),var(d,V)) > 0 end,
	{fun(_LclE) -> confirm end},
	fun(_LclE, _RmtE) -> att(cid,_RmtE) == var(c,V) end,
	[{room, fun(_LclE) -> user_code:dec(att(room,_LclE),var(d,V)) end}]},
	fun(_V) -> '_fun3'(_C,_V) end},
	{{fun(_LclE) -> user_code:selector1(att(room,_LclE),var(d,V)) == 0 end,
	{fun(_LclE) -> toolate end},
	fun(_LclE, _RmtE) -> att(cid,_RmtE) == var(c,V) end,
	[]},
	nil}
    ]).

'_fun3'(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> true end,
	{fun(_LclE) -> comission end},
	fun(_LclE, _RmtE) -> att(bid,_RmtE) == var(refc,V) end,
	[]},
	nil}
	).

'_fun2'(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> c(_C,_V) end,
	fun(_V) -> b(_C,_V) end
    ]).

'_fun1'(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> h(_C,_V) end,
	fun(_V) -> p(_C,_V) end
    ]).

