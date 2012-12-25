-module(numbers).
-compile(export_all).

% haskell Numeric, Numbers.Primes

is_pandigital(X, From, To) ->
    lists:sort(numbers:decimal_coefficients(X)) =:= lists:seq(From, To).

decimal_coefficients(0) ->
    [];
decimal_coefficients(N) when is_integer(N) ->
    [N rem 10|decimal_coefficients(N div 10)].

binary_coefficients(0) ->
    [];
binary_coefficients(N) when is_integer(N) ->
    [N rem 2|binary_coefficients(N div 2)].

factorial(0) ->
    1;
factorial(N) when N >= 1 ->
    N * factorial(N-1).
    
