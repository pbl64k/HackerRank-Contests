open System

let pn n = (3L * n * n - n) / 2L

let tst _ =
        let n = int64 (Console.ReadLine ())
        Console.WriteLine ("{0}", pn n)

let main _ =
        let t = int64 (Console.ReadLine ())
        ignore (List.map tst [1L .. t])

main ()
