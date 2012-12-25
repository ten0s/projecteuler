-module(lists2).
-compile(export_all).

is_palindrome(List) ->
    List =:= lists:reverse(List).

product(List) ->
    lists:foldl(fun(X, Acc) -> Acc * X end, 1, List).
