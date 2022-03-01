-module(food).
-compile(export_all).
-record(state, {server, name, expire_seconds}).

%by declaring server=Server, you're pattern matching the name of 
%server to Server so you can use it in the function body
start(State) ->
    spawn(?MODULE, food_loop, [State]).

food_loop(State) ->
    receive
        {Pid, Ref, cancel} ->
            %We don't actually need to do anything here
            %because if a pattern is matched, the receive loop
            %just terminates. We just send a response to let 
            %the server know we heard it, and the Ref is the match the message
            Pid ! {Ref, ok}
    after State#state.expire_seconds ->
        State#state.server ! {done, State#state.name}
    end.