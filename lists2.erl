-module(lists2).
-compile(export_all).

is_palindrome(L) ->
    L =:= lists:reverse(L).

product(L) ->
    lists:foldl(fun(X, Acc) -> Acc * X end, 1, L).

permutations([]) ->
	[[]];
permutations(L) ->
	[[H|T] || H <- L, T <- permutations(L--[H])].

uniq(L) ->
	sets:to_list(sets:from_list(L)).
