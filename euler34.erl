-module(euler34).
-export([main/0]).

main() ->
    %% exclude 1 and 2.
    LowerBound = 3,
    %% quick upper bound limit.
    UpperBound = numbers:factorial(9),
    List = lists:filter(fun(X) -> is_sum_of_factorials(X) end,
			lists:seq(LowerBound, UpperBound)),
    Sum = lists:sum(List),
    io:format("~p~n", [Sum]).

is_sum_of_factorials(N) ->
    Coeffs = numbers:decimal_coefficients(N),
    Sum = lists:foldl(fun(X, Acc) -> Acc + numbers:factorial(X) end, 0, Coeffs),
    N == Sum.
