-module(euler29).
-export([main/0]).

main() ->
	Res = length(lists:usort(
		[trunc(math:pow(A,B)) || A <- lists:seq(2,100), B <- lists:seq(2,100)]
	)),
	io:format("~p~n", [Res]).


