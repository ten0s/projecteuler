-module(euler37).
-export([main/0]).

%%
%% http://en.wikipedia.org/wiki/Truncatable_prime
%%

main() -> %% ~70 secs :(
    List = lists:filter(fun is_truncatable_prime/1, lists:filter(fun primes:is_prime/1, lists:seq(8, 750000))),
    Sum = lists:sum(List),
    io:format("~p~n", [Sum]).

is_truncatable_prime(N) ->
    is_left_truncatable_prime(N) and is_right_truncatable_prime(N).

is_left_truncatable_prime(N) ->
    case trunc_left(N) of
	0 ->
	    true;
	Truncated ->
	    case primes:is_prime(Truncated) of
		true ->
		    is_left_truncatable_prime(Truncated);
		false ->
		    false
	    end
    end.

is_right_truncatable_prime(N) ->
    case trunc_right(N) of
	0 ->
	    true;
	Truncated ->
	    case primes:is_prime(Truncated) of
		true ->
		    is_right_truncatable_prime(Truncated);
		false ->
		    false
	    end
    end.

digits_count(N) ->
    trunc(math:log10(N)) + 1.

trunc_left(0) ->
    0;
trunc_left(N) ->
    N rem trunc((math:pow(10, digits_count(N)-1))).

trunc_right(0) ->
    0;
trunc_right(N) ->
    N div 10.
