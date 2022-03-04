-module(food).
-compile(export_all).
-record(state, {server, name, expire_seconds}).

start(Name, Seconds) ->
    spawn(?MODULE, food_loop, [#state{server=self(), name=Name, expire_seconds=Seconds}]).

%interface for fridge to use
cancel(Pid) ->
    %trick: using monitor to see if process is dead
    Ref = erlang:monitor(process, Pid),
    %We default to self() here since it's always going to be a shell
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            %not dead, but will die
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            %already dead
            ok
    end.

%main food process
food_loop(State) ->
    receive
        {Pid, Ref, cancel} ->
            %We don't actually need to do anything here
            %because if a pattern is matched, the receive loop
            %just terminates. We just send an async response to let 
            %the server know we heard it, and the Ref is the match the message
            Pid ! {Ref, ok}
    after State#state.expire_seconds * 1000 ->
        io:format("~p expired!~n", [State#state.name]),
        State#state.server ! {done, State#state.name}
    end.