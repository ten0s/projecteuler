-module(euler27).
-export([main/0]).

main() ->
    {C, A, B} = max_quadratic(-999, 1000, -999, -999, 1000, {0, x, y}),
    io:format("primes: ~p a: ~p b: ~p answer: ~p~n", [C, A, B, A*B]).

quadratic(A, B, N) ->
    N*N + A*N + B.

max_quadratic(MaxX, MaxX, _, _, _, Result) ->
    Result;

max_quadratic(X, MaxX, MaxY, MinY, MaxY, Current) ->
    max_quadratic(X+1, MaxX, MinY, MinY, MaxY, Current);

max_quadratic(X, MaxX, Y, MinY, MaxY, {C0, _, _}=Current) ->
    C1 = count_primes(X, Y, 0),
    case C1 > C0 of
		true ->
	    	max_quadratic(X, MaxX, Y+1, MinY, MaxY, {C1, X, Y});
		false ->
		    max_quadratic(X, MaxY, Y+1, MinY, MaxY, Current)
	    end.

count_primes(A, B, N) ->
    case primes:is_prime(quadratic(A, B, N)) of
		true ->
	    	count_primes(A, B, N+1);
		false ->
		    N
    end.
