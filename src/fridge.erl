-module(fridge).
-compile(export_all).
-record(state, {inside}).

start() ->
    %we register the connection to the fridge event server
    %so we can access using ?MODULE. 
    %Variables starting with question marks generally are macro expansions
    %but in this case, you can also just use as a regular atom/variable
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

init() ->
    loop(#state{inside=maps:new()}).

put_in(Name, Seconds) ->
    Ref = make_ref(),
    ?MODULE ! {put_in, self(), Ref, Name, Seconds},
    receive
        {Ref, ok} ->
            ok
    after 5000 ->
        {error,timeout}
    end.

take_out(Name) ->
    Ref = make_ref(),
    ?MODULE ! {take_out, self(), Ref, Name},
    receive
        {Ref, ok} ->
            ok
    after 5000 ->
        {error,timeout}
    end.

list_inventory() ->
    ?MODULE ! {list_food}.

loop(State) ->
    receive
        {put_in, Pid, MsgRef, Name, SecondsToExpiration} ->
            Ref = food:start(Name, SecondsToExpiration),
            NewInside = maps:put(Name, Ref, State#state.inside),
            io:format("Food put in: ~p~n", [Name]),
            NewState = #state{inside=NewInside},
            Pid ! {MsgRef, ok},
            loop(NewState);
        {take_out, Pid, MsgRef, Name} ->
            Ref = erlang:monitor(process, Pid),
            Pid ! {self(), Ref, cancel},
            NewInside = maps:remove(Name, State#state.inside),
            io:format("Food taken out: ~p~n", [Name]),
            NewState = #state{inside=NewInside},
            Pid ! {MsgRef, ok},
            loop(NewState);
        {list_food} -> 
            print_list(maps:keys(State#state.inside)),
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

