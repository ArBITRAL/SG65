-module(alphatv).
-import(abel,[new_component/3,start_component/1]).
-compile(export_all).

start(N) ->
    %% define dataset, maybe read from file
    Room1 = [5,1,1,1,1,1],
    Room2 = [5,1,1,1,1,0],
    Room3 = [5,1,1,1,1,1],
    Price1 = [[0,400,500],[0,400,500],[0,400,500],[0,400,500],[0,400,500],[0,400,500]],
    Price2 = [[0,400,500],[0,400,500],[0,400,500],[0,400,500],[0,400,500],[0,400,500]],
    Price3 = [[0,400,500],[0,400,500],[0,400,500],[0,400,500],[0,400,500],[0,400,500]],
    Counter = [0,0],
    HotelLocs = [2,1],  %number of hotels at localilty l
    %% start abel, N the number of nodes of tree-based infastructure
    ok = abel:start(N),

    %% preparing attribute environment
    Cust1 = #{cid => 1, loc => 1, day => 5, dist => 3, price => 500, favh => 'undef', ref => 0, send => true, stype => 'cust'},
    Cust2 = #{cid => 2, loc => 1, day => 5, dist => 2, price => 500, favh => 'undef', ref => 0, send => true, stype => 'cust'},
    Broker1 = #{bid => 1, stype => 'broker', counter => l2prop(Counter), nh => l2prop(HotelLocs)},
    Hotel1 = #{hid => 1, stype => 'hotel', locality => 1, room => l2prop(Room1), price => l2prop2(Price1)},
    Hotel2 = #{hid => 2, stype => 'hotel', locality => 1, room => l2prop(Room2), price => l2prop2(Price2)},
    Hotel3 = #{hid => 3, stype => 'hotel', locality => 2, room => l2prop(Room3), price => l2prop2(Price3)},

    %% define interface
    IC = {cid,favh},
    IB = {bid},
    IH = {hid},

    %% creating components
    C1 = new_component(cust, Cust1, IC),
    C2 = new_component(cust, Cust2, IC),
    B = new_component(broker, Broker1, IB),
    H1 = new_component(hotel, Hotel1, IH),
    H2 = new_component(hotel, Hotel2, IH),
    H3 = new_component(hotel, Hotel3, IH),

    %% starting execution, initial behaviour is implied
    start_component(C1),
    start_component(C2),
    start_component(B),
    start_component(H1),
    start_component(H2),
    start_component(H3),
    ok.

%% convert data to work with user defined functions

l2prop(L) ->
    N = length(L),
    lists:zip(lists:seq(1,N),L).


l2prop2(L) ->
    N = length(L),
    lists:zip(lists:seq(1,N),[l2prop(L1) || L1 <- L]).
