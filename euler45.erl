-module(euler45).
-export([main/0]).

main() ->
	HexIdx = 144,
	HexVal = hexagonal(HexIdx),
	PenIdx = 166,
	PenVal = pentagonal(PenIdx),
	TriIdx = 286,
	TriVal = triangle(TriIdx),
	{_, Res, _, Res, _, Res} = solve(HexIdx, HexVal, PenIdx, PenVal, TriIdx, TriVal),
    io:format("~p~n", [Res]).

solve(HexIdx, HexVal, PenIdx, PenVal, TriIdx, TriVal) when HexVal =:= PenVal, PenVal =:= TriVal ->
	{HexIdx, HexVal, PenIdx, PenVal, TriIdx, TriVal};
solve(HexIdx, HexVal, PenIdx, PenVal, TriIdx, TriVal) when HexVal > PenVal ->
	NextPenIdx = PenIdx + 1,
	NextPenVal = pentagonal(NextPenIdx),
	solve(HexIdx, HexVal, NextPenIdx, NextPenVal, TriIdx, TriVal);
solve(HexIdx, HexVal, PenIdx, PenVal, TriIdx, TriVal) when HexVal < PenVal->
	NextHexIdx = HexIdx + 1,
	NextHexVal = hexagonal(NextHexIdx),
	solve(NextHexIdx, NextHexVal, PenIdx, PenVal, TriIdx, TriVal);
solve(HexIdx, HexVal, PenIdx, PenVal, TriIdx, TriVal) when HexVal =:= PenVal, PenVal > TriVal->
	NextTriIdx = TriIdx + 1,
	NextTriVal = triangle(NextTriIdx),
	solve(HexIdx, HexVal, PenIdx, PenVal, NextTriIdx, NextTriVal);
solve(HexIdx, HexVal, PenIdx, PenVal, TriIdx, TriVal) when HexVal =:= PenVal, PenVal < TriVal->
	NextHexIdx = HexIdx + 1,
	NextHexVal = hexagonal(NextHexIdx),
	solve(NextHexIdx, NextHexVal, PenIdx, PenVal, TriIdx, TriVal).

triangle(N) ->
	round(N * (N + 1) / 2).

pentagonal(N) ->
	round(N * (3 * N - 1) / 2).

hexagonal(N) ->
	round(N * (2 * N - 1)).
