-module(euler33).
-export([main/0]).
-compile(export_all).

main() ->
	Fractions = [{N,D} || N <- lists:seq(11,99), D <- lists:seq(11,99)],
	NonTrivialFractions = [{N,D} || {N,D} <- Fractions, is_non_trivial({N,D})],
	Product = lists:foldl(
		fun({N,D}, {NAcc,DAcc}) ->
			{N*NAcc, D*DAcc}
		end,
		{1,1},
		NonTrivialFractions
	),
	{_Num, Denom} = fold_fraction(Product),
	io:format("~p~n", [Denom]).

is_non_trivial({N, D}) ->
	NDiv = N div 10,
	NRem = N rem 10,
	DDiv = D div 10,
	DRem = D rem 10,
	if
		NRem =:= 0 orelse DRem =:= 0 -> false;
		NDiv =:= DDiv -> fold_fraction({N,D}) =:= fold_fraction({NRem,DRem}) andalso N < D;
		NDiv =:= DRem -> fold_fraction({N,D}) =:= fold_fraction({NRem,DDiv}) andalso N < D;
		NRem =:= DRem -> fold_fraction({N,D}) =:= fold_fraction({NDiv,DDiv}) andalso N < D;
		NRem =:= DDiv -> fold_fraction({N,D}) =:= fold_fraction({NDiv,DRem}) andalso N < D;
		true -> false
	end.

-spec fold_fraction({integer(), integer()}) -> {integer(), integer()}.
fold_fraction({0, 0}) ->
	{0, 0};
fold_fraction({M, N}) ->
	Gcd = numbers:gcd(M, N),
	{trunc(M/Gcd), trunc(N/Gcd)}.


