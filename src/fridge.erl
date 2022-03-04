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
            NewState = #state{inside=NewInside},
            loop(NewState);
        {take_out, Name} ->
            Pid = maps:get(Name, State#state.inside),
            Ref = erlang:monitor(process, Pid),
            Pid ! {self(), Ref, cancel},
            NewInside = maps:remove(Name, State#state.inside),
            io:format("Food taken out: ~p~n", [Name]),
            NewState = #state{inside=NewInside},
            loop(NewState);
        {list_food} -> 
            print_list(maps:keys(State#state.inside)),
            io:format(State#state.inside),
            loop(State);
        {done, Name} ->
            NewInside = maps:remove(Name, State#state.inside),
            NewState = #state{inside=NewInside},
            io:format("Food expired: ~p~n", [Name]),
            loop(NewState)
    end.

print_list([H|T]) ->
    io:format("~p~n", [H]),
    print_list(T);
print_list([]) ->
    [].

