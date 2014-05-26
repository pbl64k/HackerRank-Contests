let id x = x

let rec upto x n = if x > n then [] else x :: (upto (x + 1) n)

let pn n = (3 * n * n - n) / 2

let tst _ =
        let n = Scanf.scanf "\n%d" id
            in print_endline (string_of_int (pn n))

let main _ =
        let t = Scanf.scanf "%d" id
            in List.map tst (upto 1 t);;

main ()
