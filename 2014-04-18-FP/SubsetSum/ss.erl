
-module(ss).

% module name required by hackerrank
%-module(solution).
-export([main/0]).

read(0) -> [];
read(N) ->
    {ok, [X]} = io:fread("", "~d"),
    [X|read(N - 1)].

mktree(0, L) -> {leaf, L};
mktree(1, [{N, S}|L]) -> {{leaf, N, S, leaf}, L};
mktree(N, L) ->
    M = (N + 1) div 2,
    {LT, [{N1, S1}|L1]} = mktree(M - 1, L),
    {RT, L2} = mktree(N - M, L1),
    {{LT, N1, S1, RT}, L2}.

tfind(leaf, _) -> -1;
tfind({LT, N, S, RT}, X) ->
    if
        X == S -> N;
        X < S ->
            L1 = tfind(LT, X),
            if
                L1 == -1 -> N;
                true -> L1
            end;
        true -> tfind(RT, X)
    end.

main() ->
    {ok, [N]} = io:fread("", "~d"),
    L = read(N),
    L1 = lists:sort(fun(A, B) -> A > B end, L),
    {_, L2} = lists:foldl(fun(X, {S, L}) -> {S + X, [S + X|L]} end, {0, []}, L1),
    L3 = lists:zip(lists:seq(1, length(L2)), lists:reverse(L2)),
    {T, _} = mktree(length(L3), L3),
    {ok, [X]} = io:fread("", "~d"),
    lists:foreach(
            fun(_) ->
                {ok, [S]} = io:fread("", "~d"),
                io:fwrite("~B~n", [tfind(T, S)])
            end, lists:seq(1, X)).

