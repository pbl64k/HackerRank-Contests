open System

let rec g b f =
    match f with
    | [] -> []
    | (c :: rf) -> (List.append f (List.rev b)) :: (g (c :: b) rf)

let f cs =
    match cs with
    | [] -> []
    | (c :: cs) -> g [c] cs

let tst _ =
    let s = Console.ReadLine ()
    ignore (List.map (fun s -> Console.Write ("{0} ", (System.String (List.toArray s)))) (f (List.ofSeq s)))
    Console.WriteLine ("{0}", s)

let main _ =
    let t = int (Console.ReadLine ())
    ignore (List.map tst [1 .. t])

main ()
