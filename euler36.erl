-module(euler36).
-export([main/0]).

main() ->
    Source = lists:seq(1, 999999),
    Decimals = lists:filter(fun(X) ->
				    lists2:is_palindrome(
				      numbers:decimal_coefficients(X))
			    end, Source),
    Binaries = lists:filter(fun(X) ->
				    lists2:is_palindrome(
				      numbers:binary_coefficients(X))
			    end, Decimals),
    Sum = lists:sum(Binaries),
    io:format("~p~n", [Sum]).
