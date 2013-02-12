-module(euler42).
-export([main/0]).

main() ->
	{ok, ContentB} = file:read_file("problem42.txt"),
	ContentL = binary_to_list(ContentB),
	Words = [string:strip(Word, both, $")  || Word <- string:tokens(ContentL, ",")],
	Codes = [encode_word(Word) || Word <- Words],
	%% max code is 192 => 184 = X * (X + 1) / 2 => X^2 + X - 384 = 0 => X = 19.1 ~ 20
	Triangles = [triangle(N) || N <- lists:seq(1, 20)],
	TriangleCodes = [Code || Code <- Codes, lists:member(Code, Triangles)],
	Res = length(TriangleCodes),
	io:format("~p~n", [Res]).

encode_word(Word) ->
	lists:sum([L-$@ || L <- Word]).

triangle(N) ->
	round(N * (N + 1) / 2).
