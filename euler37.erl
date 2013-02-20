-module(euler37).
-export([main/0]).

-compile(export_all).

%%
%% http://en.wikipedia.org/wiki/Truncatable_prime
%%
main() ->
	Primes = lists:filter(fun primes:is_prime/1, lists:seq(8, 750000)),
    List = lists:filter(fun is_truncatable_prime/1, Primes),
    Sum = lists:sum(List),
	io:format("sum: ~p: list: ~p~n", [Sum, List]).

is_truncatable_prime(N) ->
    is_left_truncatable_prime(N) andalso is_right_truncatable_prime(N).

is_left_truncatable_prime(N) ->
    case trunc_left(N) of
		0 ->
		    true;
		Truncated ->
		    primes:is_prime(Truncated) andalso
			is_left_truncatable_prime(Truncated)
    end.

is_right_truncatable_prime(N) ->
    case trunc_right(N) of
		0 ->
		    true;
		Truncated ->
		    primes:is_prime(Truncated) andalso
			is_right_truncatable_prime(Truncated)
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
