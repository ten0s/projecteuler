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

-spec factorial(integer()) -> integer().
factorial(N) when N >= 0 ->
	factorial(N, 1).

factorial(0, Acc) ->
	Acc;
factorial(N, Acc) ->
	factorial(N-1, N*Acc).

-spec gcd(integer(), integer()) -> integer().
gcd(A, 0) when A > 0 ->
	A;
gcd(A, B) when A > 0, B >= 0 ->
	gcd(B, A rem B).

-spec integer_length(integer()) -> integer().
integer_length(N) ->
	trunc(math:log10(N)) + 1.
