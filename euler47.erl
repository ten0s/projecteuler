-module(euler47).
-export([main/0]).

-compile(export_all).

main() ->
	DistinctCount = 4,
	StartFrom = 2,
	Res = search(DistinctCount, StartFrom, 0),
    io:format("~p~n", [Res]).

search(DistinctCount, CurrInteger, DistinctCount) ->
	CurrInteger - DistinctCount;
search(DistinctCount, CurrInteger, Acc) ->
	case uniq_factors_count(CurrInteger) of
		DistinctCount ->
			search(DistinctCount, CurrInteger+1, Acc+1);
		_ ->
			search(DistinctCount, CurrInteger+1, 0)
	end.

uniq_factors_count(Integer) ->
	length(lists:usort(primes:prime_factors(Integer))).
