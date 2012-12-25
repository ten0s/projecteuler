-module(euler30).
-export([main/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% The maximum value for one digit is 9^5.
%% The maximum possible sum for a given number of digits is 9^5*n.
%% Next solve the equation: solve 9^5*n - 10^n = 0
%% Analytically (wolframalpha):
%% http://www.wolframalpha.com/input/?i=solve+9^5*n+-+10^n+%3D+0
%% n = 5.51257 ==> max number 9^5*n = 9^5*5.51257 ==> 325511.74593 ==> n < 326000
%% Graphically (gnuplot):
%% plot [5.5:5.52] 9**5*x, 10**x
%% x < 326000
%%
main() ->
    List = lists:filter(fun(X) -> is_sum_of_powers(X, 5) end, lists:seq(2, 326000)),
    Sum = lists:sum(List),
    io:format("~p~n", [Sum]).

is_sum_of_powers(N, P) ->
    Coeffs = numbers:decimal_coefficients(N),
    Sum = lists:foldl(fun(X, Acc) -> Acc + trunc(math:pow(X, P)) end, 0, Coeffs),
    N == Sum.
