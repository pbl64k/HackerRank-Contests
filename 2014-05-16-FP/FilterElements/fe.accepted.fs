open System

let comb xs =
    let g m x =
        match (Map.tryFind x m) with
        | None -> Map.add x 1 m
        | Some n -> Map.add x (n + 1) m
    List.fold g Map.empty xs

let f0 xs k =
    let g ((acc, m) as ac) x =
        match (Map.tryFind x m) with
        | None -> ac
        | Some n ->
            if n >= k
                then (x :: acc, Map.remove x m)
                else (acc, Map.remove x m)
    List.rev (fst (List.fold g ([], comb xs) xs))

let f xs k =
    match (f0 xs k) with
    | [] -> "-1"
    | xs -> String.Join (" ", List.map string xs)

let tst _ =
    let p = List.map int (List.ofSeq ((Console.ReadLine ()).Split [|' '|]))
    let k = List.head (List.tail p)
    let xs = List.map int (List.ofSeq ((Console.ReadLine ()).Split [|' '|]))
    Console.WriteLine ("{0}", f xs k)

let main _ =
    let t = int (Console.ReadLine ())
    ignore (List.map tst [1 .. t])

main ()
