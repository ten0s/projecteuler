-module(numbers).
-compile(export_all).

% haskell Numeric, Numbers.Primes

is_pandigital(X, From, To) ->
    lists:sort(numbers:decimal_coefficients(X)) =:= lists:seq(From, To).

decimal_coefficients(N) ->
    base_coefficients(10, N).

binary_coefficients(N) ->
    base_coefficients(2, N).

base_coefficients(_B, 0) ->
    [];
base_coefficients(B, N) when is_integer(N) ->
    [N rem B | base_coefficients(B, N div B)].

-spec factorial(integer()) -> integer().
factorial(N) when N >= 0 ->
	factorial(N, 1).

factorial(0, Acc) ->
	Acc;
factorial(N, Acc) ->
	factorial(N-1, N*Acc).

-spec gcd(integer(), integer()) -> integer().
gcd(M, 0) ->
	abs(M);
gcd(M, N) ->
	gcd(N, M rem N).

-spec lcm(integer(), integer()) -> integer().
lcm(M, N) ->
	trunc(abs(M * N) / gcd(M, N)).

-spec lcm([integer()]) -> integer().
lcm([X]) ->
	X;
lcm([X|XS]) ->
	lcm(X, lcm(XS)).

-spec integer_length(integer()) -> integer().
integer_length(N) ->
	trunc(math:log10(N)) + 1.
