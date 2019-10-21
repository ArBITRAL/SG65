*** This folder contains the execution code for the abc model alphatv.abc
** cust.erl, hotel.erl. broker.erl generated from the corresponding component definitions
** alphatv.erl, user_code.erl are hand written to provide hooks for a working scenario. 

*** The abel folder contains abel library, for conenience the abel coordinator prints the messages sent and recevied at each running component

*** Simple Run:
** Compile the code: erlc *.erl  
** Execute from Erlang shell (all the *.beam files must be under the same folder)
* > erl
* > alphatv:start(1).


