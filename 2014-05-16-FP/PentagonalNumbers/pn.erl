-module(pn).

% module name required by hackerrank
%-module(solution).
-export([main/0]).

pn(N) -> (3 * N * N - N) div 2.

main() ->
    {ok, [T]} = io:fread("", "~d"),
    lists:foreach(
            fun(_) ->
                {ok, [N]} = io:fread("", "~d"),
                io:fwrite("~B~n", [pn(N)])
            end, lists:seq(1, T)).

