
%-module(jb).

% module name required by hackerrank
-module(solution).
-export([main/0, gcd/2, lcm/2, mlcm/1]).

gcd(A, 0) -> A;
gcd(A, B) ->
    if
        A < B -> gcd(B, A);
        true -> gcd(B, A rem B)
    end.

lcm(A, B) -> (A div gcd(A, B)) * B.

mlcm(L) -> lists:foldl(fun lcm/2, 1, L).

read(0) -> [];
read(N) ->
    {ok, [X]} = io:fread("", "~d"),
    [X|read(N - 1)].

main() ->
    {ok, [N]} = io:fread("", "~d"),
    L = read(N),
    io:fwrite("~B~n", [mlcm(L)]).

