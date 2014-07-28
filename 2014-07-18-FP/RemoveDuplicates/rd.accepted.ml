module OrdChar = struct
    type t = char
    let compare = compare
end

module S = Set.Make (OrdChar)

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
            let rec imp i =
                function
                | [] -> res
                | c :: l -> res.[i] <- c; imp (i + 1) l
                in imp 0 l

let f (s, acc) c =
    if S.mem c s
        then (s, acc)
        else (S.add c s, c :: acc)

let nb str = List.rev (snd (List.fold_left f (S.empty, []) str))

let main _ =
    let str = read_line ()
        in print_endline (implode (nb (explode str)));;

main ()
