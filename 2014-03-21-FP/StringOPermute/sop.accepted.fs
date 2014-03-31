open System

let rec f xs =
    match xs with
    | [] -> []
    | (a :: b :: xs) -> b :: a :: (f xs)

let ff x = (List.ofSeq x) |> f |> List.toArray |> (fun a -> System.String a)

let tst _ =
    let str = Console.ReadLine ()
    Console.WriteLine ("{0}", ff str)

let main _ =
    let t = int (Console.ReadLine ())
    List.map tst [1 .. t]

main ()
