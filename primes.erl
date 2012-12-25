-module(primes).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%
%% @spec is_prime(integer()) -> boolean()
%%
is_prime(N) when N =< 1 ->
    false;
is_prime(N) when N > 0 ->
    case trial_division(N) of
	[] -> true;
	_  -> false
    end.

primes() ->
    void.

prime_factors() ->
    void.

%%%%%%%%%%%%%%%%%%%%
%%
%% @spec trial_division(integer()) -> [integer()]
%% 
trial_division(N) when is_integer(N), N > 0 ->
    Upper = math2:floor(math:sqrt(N)),
    lists:filter(fun(X) -> N rem X == 0 end, lists:seq(2, Upper)).
