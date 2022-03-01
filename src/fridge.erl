-module(fridge).
-compile(export_all).
%'server' is the Pid of the server, in this case it will always be self
-record(state, {server, inside = maps:new()}).

start(Pid) ->
    spawn(?MODULE, loop, [#state{server=Pid, inside=[]}]).

loop(S = #state{server=Server}) ->
    receive
        {put_in, Name, SecondsToExpiration} ->
            Ref = food:start(Name, SecondsToExpiration),
            maps:put(Name, Ref, #state.inside);
        {take_out, Name} ->
            Pid = maps:get(Name, #state.inside),
            Ref = erlang:monitor(process, Pid),
            Pid ! {self(), Ref, cancel},
            maps:remove(Name, #state.inside);
        {list_food} -> 
            io:format("~p~n", maps:keys(#state.inside))
    end.


