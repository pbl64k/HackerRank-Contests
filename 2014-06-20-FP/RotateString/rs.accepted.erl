% module name required by hackerrank
-module(solution).
-export([main/0]).

g(Acc, [], _) -> Acc;
g(Acc, [H|T], B) -> g(lists:append([Acc, [H|T], lists:reverse(B), " "]), T, [H|B]).

f([C]) -> [C];
f([H|T]) -> lists:append(g([], T, [H]), [H|T]).

tst(_) ->
    {ok, [S]} = io:fread("", "~s"),
    io:fwrite("~s~n", [f(S)]).

main() ->
    {ok, [T]} = io:fread("", "~d"),
    lists:foreach(fun tst/1, lists:seq(1, T)).
