open System

type T = L | N of (T * int64 * int64 * T)

let rec mktree (n : int64) xs =
        match (n, xs) with
        | (c, xs) when c = 0L -> (L, xs)
        | (c, (n, s)::xs) when c = 1L -> (N (L, n, s, L), xs)
        | (n, xs) ->
                let (midp : int64) = (n + 1L) / 2L
                let (l, ((n1, s1)::xs1)) = mktree (midp - 1L) xs
                let (r, xs2) = mktree (n - midp) xs1
                (N (l, n1, s1, r), xs2)

let rec tfind t x : int64 =
        match (t, x) with
        | (L, _) -> -1L
        | ((N (l, n, s, r)), x) ->
                if x = s
                    then n
                    else
                        if x < s
                            then
                                let l1 = tfind l x
                                if l1 = -1L
                                    then n
                                    else l1
                            else tfind r x

let tst t =
        let s = int64 (Console.ReadLine ())
        Console.WriteLine ("{0}", tfind t s)

let main _ =
        ignore (Console.ReadLine ())
        let str = Console.ReadLine ()
        let xs = List.map (fun x -> int64 x) (List.ofSeq (str.Split [|' '|]))
        let ys = List.rev (List.sort xs)
        let (_, y0s) = List.fold (fun (s, l) x -> (s + x, (s + x)::l)) (0L, []) ys
        let y1s = List.zip [1L .. (int64 (List.length xs))] (List.rev y0s)
        let t = int64 (Console.ReadLine ())
        let (tree, _) = mktree (int64 (List.length xs)) y1s
        ignore (List.map (fun _ -> tst tree) [1L .. t])

main ()
