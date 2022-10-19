-module(lists2).
-compile([export_all, nowarn_export_all]).

is_palindrome(L) ->
    L =:= lists:reverse(L).

product(L) ->
    lists:foldl(fun(X, Acc) -> Acc * X end, 1, L).

permutations([]) ->
	[[]];
permutations(L) ->
	[[H|T] || H <- L, T <- permutations(L--[H])].
