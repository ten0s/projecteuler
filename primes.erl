-module(primes).
-compile(export_all).

-spec is_prime(pos_integer()) -> boolean().
is_prime(N) when N < 1 ->
	exit(badagr);
is_prime(1) ->
    true;
is_prime(2) ->
	true;
is_prime(3) ->
	true;
is_prime(N) when N > 0 ->
	trial_division(N).

primes() ->
    void.

-spec prime_factors(pos_integer()) -> [pos_integer()].
prime_factors(N) when N < 1 ->
	exit(badarg);
prime_factors(N) when N =:= 1 ->
	[];
prime_factors(N) when N > 0 ->
	prime_factors(N, 2, []).

%% ===================================================================
%% Internal
%% ===================================================================

trial_division(N) ->
    Upper = math2:floor(math:sqrt(N)),
	trial_division(N, 2, Upper).

trial_division(N, Upper, Upper) ->
	N rem Upper =/= 0;
trial_division(N, Curr, Upper) ->
	N rem Curr =/= 0 andalso
	trial_division(N, Curr + 1, Upper).

prime_factors(1, _Prime, Factors) ->
	Factors;
prime_factors(N, Prime, Factors) ->
	case N rem Prime =:= 0 of
		true ->
			prime_factors(N div Prime, Prime, [Prime | Factors]);
		false ->
			NextPrime = next_prime(Prime + 1),
			prime_factors(N, NextPrime, Factors)
	end.

next_prime(N) ->
	case is_prime(N) of
		true ->
			N;
		false ->
			next_prime(N + 1)
	end.
