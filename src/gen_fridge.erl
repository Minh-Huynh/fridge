-module(gen_fridge).
-behavior(gen_server).
-compile(export_all).
-record(state, {inside}).

start() ->
    %we register the connection to the fridge event server
    %so we can access using ?MODULE. 
    %Variables starting with question marks generally are macro expansions
    %but in this case, you can also just use as a regular atom/variable
    %register(?MODULE, Pid=spawn(?MODULE, init, [])),
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) -> 
    NewState = #state{inside=maps:new()},
    {ok, NewState}.

%init() ->
%    loop(#state{inside=maps:new()}).

%We expect a reply, so sync
put_in(Pid, Name, Seconds) ->
    gen_server:call(Pid, {put_in, Name, Seconds}).
    %Ref = make_ref(),
    %?MODULE ! {put_in, self(), Ref, Name, Seconds},
    %receive
    %    {Ref, ok} ->
    %        ok
    %after 5000 ->
    %    {error,timeout}
    %end.

take_out(Pid, Name) ->
    gen_server:call(Pid, {take_out, Name}).
    %Ref = make_ref(),
    %?MODULE ! {take_out, self(), Ref, Name},
    %receive
    %    {Ref, ok} ->
    %        ok
    %after 5000 ->
    %    {error,timeout}
    %end.

list_inventory(Pid) ->
    gen_server:cast(Pid, list_food).


%last argument is the current state record of the server
handle_call({put_in, Name, Seconds}, _From, Items) ->
    %like 'make_cat' method
    Ref = food:start(Name, Seconds),
    NewItems = maps:put(Name, Ref, Items#state.inside),
    io:format("Food put in: ~p~n", [Name]),
    NewState = #state{inside=NewItems},
    {reply, Ref, NewState};

handle_call({take_out, Name}, _From, Items) ->
    Pid = maps:get(Name, Items#state.inside),
    food:cancel(Pid),
    NewInside = maps:remove(Name, Items#state.inside),
    io:format("Food taken out: ~p~n", [Name]),
    NewState = #state{inside=NewInside},
    %{response_atom, sendToClient, updatedState}
    {reply, Name, NewState}.

handle_cast(list_food, Items) ->
    print_list(maps:keys(Items#state.inside)),
    {noreply, Items};

handle_cast({done, Name}, Items) ->
    NewInside = maps:remove(Name, Items#state.inside),
    NewState = #state{inside=NewInside},
    io:format("Food expired: ~p~n", [Name]),
    {noreply, NewState}.

%loop(State) ->
%    receive
%        {put_in, Pid, MsgRef, Name, SecondsToExpiration} ->
%            Ref = food:start(Name, SecondsToExpiration),
%            NewInside = maps:put(Name, Ref, State#state.inside),
%            io:format("Food put in: ~p~n", [Name]),
%            NewState = #state{inside=NewInside},
%            Pid ! {MsgRef, ok},
%            loop(NewState);
%        {take_out, Pid, MsgRef, Name} ->
%            Ref = erlang:monitor(process, Pid),
%            Pid ! {self(), Ref, cancel},
%            NewInside = maps:remove(Name, State#state.inside),
%            io:format("Food taken out: ~p~n", [Name]),
%            NewState = #state{inside=NewInside},
%            Pid ! {MsgRef, ok},
%            loop(NewState);
%        {list_food} -> 
%            print_list(maps:keys(State#state.inside)),
%            loop(State);
%        {done, Name} ->
%            NewInside = maps:remove(Name, State#state.inside),
%            NewState = #state{inside=NewInside},
%            io:format("Food expired: ~p~n", [Name]),
%            loop(NewState)
%    end.

%%%%Private functions
print_list([H|T]) ->
    io:format("~p~n", [H]),
    print_list(T);
print_list([]) ->
    [].

