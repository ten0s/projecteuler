-module(euler28).
-export([main/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 21 22 23 24 25
%% 20  7  8  9 10
%% 19  6  1  2 11
%% 18  5  4  3 12
%% 17 16 15 14 13
%%
%% ---
%%  /| a(n) = odd(n)^2 = odd(n-1)^2 + 4odd(n-1) + 4
%%
%%
%%  \| b(n) = a(n-1) + odd(n-1) + 1 = odd(n-1)^2 + odd(n-1) + 1
%% ---
%%
%% |/  c(n) = b(n) + odd(n-1) + 1 = odd(n-1)^2 + 2odd(n-1) + 2
%% ---
%%
%% ---
%% |\  d(n) = c(n) + odd(n-1) + 1 = odd(n-1)^2 + 3odd(n-1) + 3
%%
%%  n | 0 | 1 | 2 | 3 | 4 | 5 | ...
%% --------------------------------
%% odd|-1 | 1 | 3 | 5 | 7 | 9 | ...
%%
%% s(1) = 1
%% s(n) = a(n) + b(n) + c(n) + d(n) = 4odd(n-1)^2 + 10odd(n-1) + 10
%%

main() ->
    Res = s(trunc((1001+1)/2)),
    io:format("~p~n", [Res]).

s(1) -> 1;
s(N) when is_integer(N) ->
    Odd = odd(N-1),
    4*Odd*Odd + 10*Odd + 10 + s(N-1).

odd(N) ->
    2*N - 1.