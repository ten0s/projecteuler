-module(euler44).
-export([main/0]).

main() ->
	%% easy to parallelize :)
	{ok, Diff} = solve(1, 10000, 1, 10000),
    io:format("~p~n", [Diff]).

solve(_StartIdx, _MaxIdx, MaxStep, MaxStep) ->
	{error, not_found};
solve(StartIdx, MaxIdx, Step, MaxStep) ->
	case find(StartIdx, MaxIdx, Step) of
		{ok, Diff} ->
			{ok, Diff};
		{error, not_found} ->
			solve(StartIdx, MaxIdx, Step + 1, MaxStep)
	end.

find(MaxIdx, MaxIdx, _Step) ->
	{error, not_found};
find(Idx, MaxIdx, Step) ->
	P1 = pentagon(Idx),
	P2 = pentagon(Idx + Step),
	Sum = P1 + P2,
	Diff = abs(P1 - P2),
	case is_pentagon(Sum) andalso is_pentagon(Diff) of
		true ->
			{ok, Diff};
		false ->
			find(Idx + 1, MaxIdx, Step)
	end.

pentagon(N) ->
	round(N * (3 * N - 1) / 2).

is_pentagon(P) ->
	%% solve 3X^2 - X - 2P = 0, X > 0
	X = (1 + math:sqrt(1 + 24 * P)) / 6,
	round(X) == X.
