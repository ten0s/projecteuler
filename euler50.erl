-module(euler50).
-export([main/0]).

main() ->
	[{Res, _} | _] = search_sums(1000000),
    io:format("~p~n", [Res]).

search_sums(MaxSum) ->
	search_sums(2, 2, 0, 0, [], MaxSum).

search_sums(InitPrime, _CurrPrime, _Sum, _Count, Acc, MaxSum) when InitPrime >= MaxSum ->
	lists:sort(fun({_, C1}, {_, C2}) -> C1 > C2 end, Acc);
search_sums(InitPrime, _CurrPrime, Sum, _Count, Acc, MaxSum) when Sum > MaxSum ->
	NewInitPrime = primes:next_prime(InitPrime),
	search_sums(NewInitPrime, NewInitPrime, 0, 0, Acc, MaxSum);
search_sums(InitPrime, CurrPrime, Sum, Count, Acc, MaxSum) ->
	NewCurrPrime = primes:next_prime(CurrPrime),
	NewSum = CurrPrime + Sum,
	NewCount = Count + 1,
	NewAcc =
		case NewSum < MaxSum andalso primes:is_prime(NewSum) of
			true ->
				[{NewSum, NewCount} | Acc];
			false ->
				Acc
		end,
	search_sums(InitPrime, NewCurrPrime, NewSum, NewCount, NewAcc, MaxSum).
