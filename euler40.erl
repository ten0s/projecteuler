-module(euler40).
-export([main/0]).

main() ->
	Sequence = lists:flatten([integer_to_list(X) || X <- lists:seq(1, 200000)]),
	Digits = [lists:nth(N, Sequence) - $0 || N <- [1, 10, 100, 1000, 10000, 100000, 1000000]],
    Product = lists2:product(Digits),
    io:format("~p~n", [Product]).
