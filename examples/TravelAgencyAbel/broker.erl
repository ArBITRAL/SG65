-module(broker).
-import(abel,[prefix/3,choice/3,parallel/3,call/3,att/2,var/2,msg/2]).
-export([init_beh/2]).
init_beh(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> p(_C,_V) end,
	fun(_V) -> c(_C,_V) end
    ]).

p(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == acms end,
	{x,c,l,d,p},
	[{counter, fun(_LclE,_M) -> user_code:zero(att(counter,_LclE),msg(2,_M)) end}]},
	fun(_V) -> '_fun1'(_C,_V) end}
	).

f(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> true end,
	{fun(_LclE) -> acms end,fun(_LclE) -> var(c,V) end,fun(_LclE) -> var(d,V) end,fun(_LclE) -> att(bid,_LclE) end},
	fun(_LclE, _RmtE) -> att(stype,_RmtE) == hotel andalso att(locality,_RmtE) == var(l,V) end,
	[]},
	fun(_V) -> '_fun2'(_C,_V) end}
	).

a(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> user_code:selector1(att(counter,_LclE),var(c,V)) < user_code:selector1(att(nh,_LclE),var(l,V)) end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == offer andalso msg(2,_M) == var(c,V) andalso msg(5,_M) =< var(p,V) end,
	{x,cust,h,l,op},
	[]},
	fun(_V) -> '_fun3'(_C,_V) end}
	).

s(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> true end,
	{fun(_LclE) -> offer end,fun(_LclE) -> var(h,V) end,fun(_LclE) -> var(l,V) end,fun(_LclE) -> var(op,V) end,fun(_LclE) -> att(bid,_LclE) end},
	fun(_LclE, _RmtE) -> att(cid,_RmtE) == var(c,V) end,
	[{counter, fun(_LclE) -> user_code:inc(att(counter,_LclE),var(c,V)) end}]},
	nil}
	).

u(_C,V) -> 
    choice(_C,V,[
	{{fun(_LclE) -> user_code:selector1(att(counter,_LclE),var(c,V)) < user_code:selector1(att(nh,_LclE),var(l,V)) end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == offer andalso msg(5,_M) > var(p,V) end,
	{x,cust,h,l,op},
	[{counter, fun(_LclE,_M) -> user_code:inc(att(counter,_LclE),var(c,V)) end}]},
	fun(_V) -> u(_C,[]) end},
	{{fun(_LclE) -> user_code:selector1(att(counter,_LclE),var(c,V)) < user_code:selector1(att(nh,_LclE),var(l,V)) end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == nooffer andalso msg(2,_M) == var(c,V) end,
	{x,cust},
	[{counter, fun(_LclE,_M) -> user_code:inc(att(counter,_LclE),var(c,V)) end}]},
	fun(_V) -> u(_C,[]) end}
    ]).

r(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> user_code:selector1(att(counter,_LclE),var(c,V)) == user_code:selector1(att(nh,_LclE),var(l,V)) end,
	{fun(_LclE) -> finish end,fun(_LclE) -> var(c,V) end},
	fun(_LclE, _RmtE) -> att(cid,_RmtE) == var(c,V) end,
	[]},
	nil}
	).

c(_C,V) -> 
    prefix(_C,V,{{fun(_LclE) -> true end,
	fun(_LclE, _M, _RmtE) -> msg(1,_M) == comission andalso att(stype,_RmtE) == hotel end,
	{x},
	[]},
	nil}
	).

'_fun3'(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> s(_C,_V) end,
	fun(_V) -> a(_C,_V) end
    ]).

'_fun2'(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> a(_C,_V) end,
	fun(_V) -> u(_C,_V) end,
	fun(_V) -> r(_C,_V) end
    ]).

'_fun1'(_C,V) -> 
    parallel(_C,V,[
	fun(_V) -> f(_C,_V) end,
	fun(_V) -> p(_C,_V) end
    ]).

