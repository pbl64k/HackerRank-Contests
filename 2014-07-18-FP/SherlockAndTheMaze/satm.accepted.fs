open System

let mm = 1000000007L

let rec dpa: Lazy<int64> array = [| for n in 1 .. 100 do for m in 1 .. 100 do for k in 0 .. 100 -> lazy (f (n, m, k)) |]
and f =
    function
    | (_, 1, 0) -> 1L
    | (_, 1, _) | (1, _, _) -> 0L
    | (n, m, k) -> ((g (n - 1) m k) + (g m (n - 1) (k - 1))) % mm
and dp n m k = dpa.[k + (m - 1) * 101 + (n - 1) * 10100]
and g n m k =
    if n > 0 && m > 0 && k >= 0
        then (dp n m k).Force ()
        else 0L

let s n m k = List.fold (fun a b -> (a + b) % mm) 0L (List.map (fun x -> (g n m x + g m n x) % mm) [0 .. k])

let tst _ =
    let (n :: m :: k :: _) = List.map int (List.ofSeq ((Console.ReadLine ()).Split [|' '|]))
    if n = 1 && m = 1
        then ignore (Console.WriteLine ("{0}", 1))
        else ignore (Console.WriteLine ("{0}", s n m k))

let main _ =
    let t = int (Console.ReadLine ())
    ignore (List.map tst [1 .. t])

main ()

