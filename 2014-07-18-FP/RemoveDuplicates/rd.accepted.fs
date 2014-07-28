open System

let f ((s, acc) as p) c =
    if Set.contains c s
        then p
        else (Set.add c s, c :: acc)

let nb str = str |> List.ofSeq |> (List.fold f (Set.empty, [])) |> snd |> List.rev |> List.toArray |> (fun x -> System.String x)

let main _ =
    let str = Console.ReadLine ()
    Console.WriteLine ("{0}", nb str)

main ()
