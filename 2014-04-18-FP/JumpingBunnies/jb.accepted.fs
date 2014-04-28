open System

let rec gcd a b =
        if b = 0UL
            then a
            else
                if a < b
                    then gcd b a
                    else gcd b (a % b)

let lcm a b = (a / (gcd a b)) * b

let main _ =
        ignore (Console.ReadLine ())
        let str = Console.ReadLine ()
        let xs = List.map uint64 (List.ofSeq (str.Split [|' '|]))
        Console.WriteLine ("{0}", List.fold lcm 1UL xs)

main ()
