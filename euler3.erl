-module(euler3).
-export([main/0]).

main() ->
	Res = lists:max(primes:prime_factors(600851475143)),
	io:format("~p~n", [Res]).
