-module(euler39).
-export([main/0]).

-define(MAX, 401).

main() ->
    Acc = solution(1, 1, 1, array:new([{size, 1001}, {fixed, true}, {default, 0}])),
    {Res, _} = array:foldl(
		fun (Perim, Freq, {MaxPerim, MaxFreq}) ->
			case Freq > MaxFreq of
				true ->
					{Perim, Freq};
			    false ->
					{MaxPerim, MaxFreq}
			end
		end,
		{0, 0},
		Acc
	),
	io:format("~p~n", [Res]).

solution(?MAX, 1, 1, Acc) ->
    Acc;
solution(A, B, ?MAX, Acc) ->
    solution(A, B+1, 1, Acc);
solution(A, ?MAX, 1, Acc) ->
    solution(A+1, 1, 1, Acc);
solution(A, B, C, Acc) ->
    NewAcc =
	case is_triangle(A, B, C) of
	    true ->
		P = perimeter(A, B, C),
		case P =< 1000 of
		    true ->
			case is_right_triangle(A, B, C) of
			    true ->
				array:set(P, array:get(P, Acc)+1, Acc);
			    false ->
				Acc
			end;
		    false ->
			Acc
		end;
	    false ->
		Acc
	end,
    solution(A, B, C+1, NewAcc).

is_triangle(A, B, C) ->
    (A < B + C) and
    (B < A + C) and
    (C < A + B).

is_right_triangle(A, B, C) ->
    A*A + B*B =:= C*C.

perimeter(A, B, C) ->
    A + B + C.
