-module(food).
-compile(export_all).

start(Name, Expiration) ->
    spawn(?MODULE, )

food_loop(Name, SecondsToExpiration) ->
    %figure out the time here
    %have user pass in days until expiration
    receive
        {take_out} ->;
    after SecondsToExpiration ->
    end.