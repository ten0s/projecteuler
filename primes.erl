-module(primes).
-compile(export_all).

-spec is_prime(pos_integer()) -> boolean().
is_prime(N) when N < 1 ->
	exit(badagr);
is_prime(N) when N =:= 1 ->
    true;
is_prime(N) when N > 0 ->
    case trial_division(N) of
		[] ->
			true;
		_  ->
			false
    end.

primes() ->
    void.

-spec prime_factors(pos_integer()) -> [pos_integer()].
prime_factors(N) when N < 1 ->
	exit(badarg);
prime_factors(N) when N =:= 1 ->
	[];
prime_factors(N) when N > 0 ->
	prime_factors(N, 2, []).

prime_factors(1, _Prime, Factors) ->
	lists:sort(Factors);
prime_factors(N, Prime, Factors) ->
	case N rem Prime =:= 0 of
		true ->
			prime_factors(N div Prime, Prime, [Prime | Factors]);
		false ->
			NextPrime = next_prime(Prime),
			prime_factors(N, NextPrime, Factors)
	end.

next_prime(N) ->
	next_prime(N, N).

next_prime(N, N) ->
	next_prime(N+1, N);
next_prime(M, N) ->
	case is_prime(M) of
		true ->
			M;
		false ->
			next_prime(M+1, N)
	end.

-spec trial_division(pos_integer()) -> [integer()].
trial_division(N) when is_integer(N), N > 0 ->
    Upper = math2:floor(math:sqrt(N)),
    lists:filter(fun(X) -> N rem X == 0 end, lists:seq(2, Upper)).
