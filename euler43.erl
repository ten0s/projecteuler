-module(euler43).
-export([main/0]).

main() ->
	Perms = lists2:permutations([0,1,2,3,4,5,6,7,8,9]),
	List = filter_perms(Perms),
	Sum = lists:sum(List),
    io:format("~p~n", [Sum]).

filter_perms(Perms) ->
	filter_perms(Perms, []).

filter_perms([], Acc) ->
	Acc;
filter_perms([Perm | Perms], Acc) ->
	case check_perm(Perm) of
		true ->
			Int = list_to_integer([I+$0 || I <- Perm]),
			filter_perms(Perms, [Int | Acc]);
		false ->
			filter_perms(Perms, Acc)
	end.

check_perm(Perm) ->
	sub(2, 3,  4, Perm) rem 2  =:= 0 andalso
	sub(3, 4,  5, Perm) rem 3  =:= 0 andalso
	sub(4, 5,  6, Perm) rem 5  =:= 0 andalso
	sub(5, 6,  7, Perm) rem 7  =:= 0 andalso
	sub(6, 7,  8, Perm) rem 11 =:= 0 andalso
	sub(7, 8,  9, Perm) rem 13 =:= 0 andalso
	sub(8, 9, 10, Perm) rem 17 =:= 0.

sub(I1, I2, I3, Perm) ->
	D1 = lists:nth(I1, Perm),
	D2 = lists:nth(I2, Perm),
	D3 = lists:nth(I3, Perm),
	D1*100 + D2*10 + D3.


