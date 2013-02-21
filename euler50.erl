-module(euler50).
-export([main/0]).

main() ->
	[{Res, _} | _] = search_sums(1000000),
    io:format("~p~n", [Res]).

search_sums(MaxSum) ->
	search_all_sums(2, [], MaxSum).

search_all_sums(InitPrime, Acc, MaxSum) when InitPrime >= MaxSum ->
	lists:sort(fun({_, C1}, {_, C2}) -> C1 > C2 end, Acc);
search_all_sums(InitPrime, Acc, MaxSum) ->
	NewAcc = search_sub_sums(InitPrime, 0, 0, Acc, MaxSum),
	NewInitPrime = primes:next_prime(InitPrime),
	search_all_sums(NewInitPrime, NewAcc, MaxSum).

search_sub_sums(_CurrPrime, Sum, _Count, Acc, MaxSum) when Sum > MaxSum ->
	Acc;
search_sub_sums(CurrPrime, Sum, Count, Acc, MaxSum) ->
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
	search_sub_sums(NewCurrPrime, NewSum, NewCount, NewAcc, MaxSum).
