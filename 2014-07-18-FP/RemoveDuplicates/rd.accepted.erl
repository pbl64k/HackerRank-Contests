% module name required by hackerrank
-module(solution).
-export([main/0]).

f(C, {S, Acc}) ->
    B = sets:is_element(C, S),
    if
        B -> {S, Acc};
        true -> {sets:add_element(C, S), [C|Acc]}
    end.

nb(Str) -> lists:reverse(element(2, lists:foldl(fun f/2, {sets:new(), []}, Str))).

main() ->
    {ok, [Str]} = io:fread("", "~s"),
    io:fwrite("~s~n", [nb(Str)]).

