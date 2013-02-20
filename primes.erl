-module(primes).
-export([
	is_prime/1,
	prime_factors/1,
	next_prime/1
]).

-type prime() :: pos_integer().

-spec is_prime(pos_integer()) -> boolean().
is_prime(N) when N < 1 ->
	exit(badagr);
is_prime(1) ->
    false;
is_prime(2) ->
	true;
is_prime(3) ->
	true;
is_prime(N) when N > 0 ->
	trial_division(N).

-spec prime_factors(pos_integer()) -> [prime()].
prime_factors(N) when N < 1 ->
	exit(badarg);
prime_factors(N) when N =:= 1 ->
	[];
prime_factors(N) when N > 0 ->
	prime_factors(N, 2, []).

-spec next_prime(pos_integer()) -> prime().
next_prime(N) when N < 1 ->
	exit(badarg);
next_prime(N) ->
	next_prime(N, N).

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
			prime_factors(N, next_prime(Prime, Prime), Factors)
	end.

next_prime(N, N) ->
	next_prime(N + 1, N);
next_prime(M, N) ->
	case primes:is_prime(M) of
		true ->
			M;
		false ->
			next_prime(M + 1, N)
	end.
