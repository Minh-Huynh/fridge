-module(fridge).
-compile(export_all).
-record(state, {server, inside = []}).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
        {put_in, Name, expiration} ->;
        {take_out, Name} ->;
        {list_food} -> ;
    after 5000 ->;
    end.
