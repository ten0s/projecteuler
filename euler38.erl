-module(euler38).
-export([main/0]).

%%
%% If X=1 and [1..9] the product is 123456789. It's the pandigital,
%% but abviously the mininal.
%% If X=4 and [1..6] the product is greater than 987654321, that sets
%% 6 to be the upper bound for N.
%% If X=10000 and [1..2] the product is also greater than 987654321, that
%% sets 100000 to be the upper bound for X.
%%

main() ->
    Pandigitals = [P ||
	       X <- lists:seq(2, 100000),
	       N <- lists:seq(2, 6),
	       begin P = concatenated_product(X, lists:seq(1, N)),
		     P =< 987654321
	       end,
	       numbers:is_pandigital(P, 1, 9)
	   ],
    Result = hd(lists:reverse(lists:sort(Pandigitals))),
    io:format("~p~n", [Result]).

concatenated_product(X, List) ->
    ConcatList = lists:map(fun(Y) -> integer_to_list(X*Y) end, List),
    list_to_integer(lists:flatten(ConcatList)).
