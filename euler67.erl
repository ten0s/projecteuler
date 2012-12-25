-module(euler67).
-export([main/0]).

%%
%% Project Euler -- Problem 67
%% http://projecteuler.net/problem=67
%%
%% erl -noshell -s euler67 main -s init stop
%%

%%
%% main() -> void()
%%
main() ->
    case file:read_file("problem67.txt") of
		{ok, Binary} ->
			Lines = lists:reverse(lines(binary_to_list(Binary))),
			Ints = [[list_to_integer(W) || W <- words(L)] || L <- Lines],
			Triangle = [address(0, L) || L <- Ints],
			[{Result, _Path} | _] = solve(Triangle),
			io:format("~p~n", [Result]);
		{error, Reason} ->
			case Reason of
				enoent -> io:format("Error: The file does not exist.~n");
				_      -> io:format("Error: Unknown error.~n")
			end,
			halt(1)
	    end.

%% lines(string()) -> [string()]
%%    Splits string to list of lines by line breaks.
%%    Example:
lines(L) ->
    string:tokens(L, "\r\n").

%% words(string()) -> [string()]
%%    Splits string to words by spaces.
%%    Example: words("1 2 3 4") ==> ["1","2","3","4"]
words(L) ->
    string:tokens(L, " ").

%% address(Addr, [X]) -> [{X, [Addr]}]
%%    Sets address list to each list item.
%%    Example: address(0, [10,2,13,4]) => [{10,[0]}, {2,[1]}, {13,[2]}, {4, [3]}]
address(_, []) -> [];
address(Addr, [H|T]) -> [{H, [Addr]} | address(Addr+1, T)].

%% shrink([{X, [Addr]}]) -> [{X, [Addr]}]
%%    Shrinks value/address list pairs.
%%    Example: shrink([{8,[0]},{5,[1]},{9,[2]},{3,[3]}]) => [{8,[0]},{9,[2]},{9,[2]}]
shrink([]) ->
	[];
shrink([_]) ->
	[];
shrink([X1,X2|XS]) ->
	[choose(X1, X2) | shrink([X2|XS])].

%% choose({X, [Addr]}, {X, [Addr]}) -> {X, [Addr]}
%%     Chooses maximum between tuples using keys.
%%     Example: choose({2,[5]}, {7,[1]}) => {7,[1]}
choose({V1,_}=X1, {V2,_}=X2) ->
    case V1 > V2 of
		true  -> X1;
		false -> X2
    end.

%% solve([{X, [Addr]}]) -> [{X, [Addr]}]
%%     Solves the triangle using the dynamic programming technique.
%%     Shrinks each row and combines (add keys) it with the next one,
%%     collecting addresses at the same time.
solve([X]) ->
	X;
solve([T,N|XS]) ->
	T1 = shrink(T),
	N1 = lists:zipwith(fun({V1,A1}, {V2,A2}) -> {V1+V2, A2++A1} end, T1, N),
	solve([N1|XS]).
