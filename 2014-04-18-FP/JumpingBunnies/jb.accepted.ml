let id x = x

let rec upto x n =
    if x > n
        then []
        else x :: (upto (x + 1) n)

let rec gcd a b =
    if b = 0
        then a
        else
            if a < b
                then gcd b a
                else gcd b (a mod b)

let lcm a b = (a / (gcd a b)) * b

let main _ =
    let t = Scanf.scanf "%d\n" id
        in let xs = List.map
                (function x ->
                    if x = 1
                        then Scanf.scanf "%d" id
                        else Scanf.scanf " %d" id) (upto 1 t)
            in print_endline (string_of_int (List.fold_left lcm 1 xs));;

main ()

