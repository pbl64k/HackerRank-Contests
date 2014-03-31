% module name required by hackerrank
-module(solution).
-export([main/0]).

f([A|[B|T]])-> [B|[A|f(T)]];
f(X) -> X.

main() ->
    {ok, [X]} = io:fread("", "~d"),
    lists:foreach(fun(Z) -> io:fwrite("~s", [f(io:get_line(""))]) end, lists:seq(1, X)).
