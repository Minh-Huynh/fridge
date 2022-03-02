-module(fridge).
-compile(export_all).
-record(state, {inside}).

start() ->
    spawn(?MODULE, init, []).

init() ->
    loop(#state{inside=maps:new()}).

loop(State) ->
    receive
        {put_in, Name, SecondsToExpiration} ->
            Ref = food:start(Name, SecondsToExpiration),
            NewInside = maps:put(Name, Ref, State#state.inside),
            io:format("Food put in: ~p~n", [Name]),
            State#state{inside=NewInside},
            loop(State);
        {take_out, Name} ->
            Pid = maps:get(Name, State#state.inside),
            Ref = erlang:monitor(process, Pid),
            Pid ! {self(), Ref, cancel},
            maps:remove(Name, State#state.inside),
            io:format("Food taken out: ~p~n", [Name]),
            loop(State);
        {list_food} -> 
            %%Currently having trouble inserting values into state record
            io:format("~p~n", maps:keys(State#state.inside)),
            loop(State)
    end.


