-module(euler52).
-export([main/0]).

main() ->
    Res = find(1),
    io:format("~p~n", [Res]).

find(X) ->
    Xstr = lists:sort(integer_to_list(X)),
    X2str = lists:sort(integer_to_list(X*2)),
    case Xstr =:= X2str of
        true ->
            X3str = lists:sort(integer_to_list(X*3)),
            case Xstr =:= X3str of
                true ->
                    X4str = lists:sort(integer_to_list(X*4)),
                    case Xstr =:= X4str of
                        true ->
                            X5str = lists:sort(integer_to_list(X*5)),
                            case Xstr =:= X5str of
                                true ->
                                    X6str = lists:sort(integer_to_list(X*6)),
                                    case Xstr =:= X6str of
                                        true ->
                                            X;
                                        _ ->
                                            find(X+1)
                                    end;
                                _ ->
                                    find(X+1)
                            end;
                        _ ->
                            find(X+1)
                    end;
                _ ->
                    find(X+1)
            end;
        _ ->
            find(X+1)
    end.
