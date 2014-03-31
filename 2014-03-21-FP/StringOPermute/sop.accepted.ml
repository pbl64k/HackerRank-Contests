let rec f xs =
    match xs with
    | (a :: b :: xs) -> b :: a :: (f xs)
    | xs -> xs

(* OCaml is SO painful without something like Batteries Included. *)
(* explode/implode lifted from http://caml.inria.fr/mantis/view.php?id=5367 *)
let explode s =
    let rec exp i l =
        if i < 0
            then l
            else exp (i - 1) (s.[i] :: l)
        in exp (String.length s - 1) []

let implode l =
    let res = String.create (List.length l)
        in
            let rec imp i = function |[] -> res| c :: l -> res.[i] <- c; imp (i + 1) l
                in imp 0 l

let ff x = implode (f (explode x))

let tst _ =
    let str = read_line ()
        in print_endline (ff str)

let rec upto x n =
    if x > n
        then []
        else x :: (upto (x + 1) n)

let main () =
    let t = int_of_string (read_line ())
        in List.map tst (upto 1 t);;

main ()
