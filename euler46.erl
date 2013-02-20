-module(euler46).
-export([main/0]).

-compile(export_all).

main() ->
	{busted, CompositeOdd} = bust_conjecture(9),
    io:format("~p~n", [CompositeOdd]).

bust_conjecture(CompositeOdd) ->
	case find_prime_integers(CompositeOdd, 2, 1) of
		{found, _, _} ->
			bust_conjecture(next_composite_odd(CompositeOdd));
		not_found ->
			{busted, CompositeOdd}
	end.

find_prime_integers(CompositeOdd, Prime, Integer) when CompositeOdd >= Prime ->
	case find_integers(CompositeOdd, Prime, Integer) of
		{found, NewInteger} ->
			{found, Prime, NewInteger};
		not_found ->
			find_prime_integers(CompositeOdd, next_prime(Prime), 1)
	end;
find_prime_integers(_, _, _) ->
	not_found.

find_integers(CompositeOdd, Prime, Integer) ->
	Value = Prime + 2 * round(math:pow(Integer, 2)),
	if
		Value =:= CompositeOdd ->
			{found, Integer};
		Value  >= CompositeOdd ->
			not_found;
		Value  =< CompositeOdd ->
			find_integers(CompositeOdd, Prime, Integer + 1)
	end.

next_composite_odd(N) ->
	NextOdd = N + 2,
	case primes:is_prime(NextOdd) of
		true ->
			next_composite_odd(NextOdd);
		false ->
			NextOdd
	end.

next_prime(N) ->
	next_prime(N, N).

next_prime(N, N) ->
	next_prime(N+1, N);
next_prime(M, N) ->
	case primes:is_prime(M) of
		true ->
			M;
		false ->
			next_prime(M+1, N)
	end.
