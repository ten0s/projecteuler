-module(euler32).
-export([main/0]).

main() ->
	Perms = lists2:permutations("123456789"),
	List = filter_perms(Perms),
	Sum = lists:sum(List),
    io:format("~p~n", [Sum]).

filter_perms(Perms) ->
	filter_perms(Perms, []).

filter_perms([], Products) ->
	lists:usort(Products);
filter_perms([Perm|Perms], Products) ->
	case check_perm(Perm) of
		{true, Product} ->
			filter_perms(Perms, [Product|Products]);
		false ->
			filter_perms(Perms, Products)
	end.

check_perm(PermL) ->
	%% the idea here is that minimal product lenght is 4, due to 1*2345=2345.
	{MultipsL, ProductL} = lists:split(5, PermL),
	ProductI = list_to_integer(ProductL),
	check_multips(1, 5, MultipsL, ProductI).

check_multips(Max, Max, _, _) ->
	false;
check_multips(I, Max, MultipsL, ProductI) ->
	{MultiplicandL, MultiplierL} = lists:split(I, MultipsL),
	MultiplicandI = list_to_integer(MultiplicandL),
	MultiplierI = list_to_integer(MultiplierL),
	case MultiplicandI * MultiplierI =:= ProductI of
		true ->
			{true, ProductI};
		false ->
			check_multips(I+1, Max, MultipsL, ProductI)
	end.

