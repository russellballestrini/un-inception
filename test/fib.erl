-module(fib).
-export([main/1]).

fib(N) when N =< 1 -> N;
fib(N) -> fib(N-1) + fib(N-2).

main(_) ->
    lists:foreach(fun(I) ->
        io:format("fib(~p) = ~p~n", [I, fib(I)])
    end, lists:seq(0, 10)).
