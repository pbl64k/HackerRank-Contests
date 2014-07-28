open System

type 'a pheap = PhEmpty of ('a -> 'a -> bool) | Ph of ('a -> 'a -> bool) * int * 'a * ('a pheap) list

let ph_empty f = PhEmpty f

let ph_sing f e = Ph (f, 1, e, [])

let ph_size =
    function
    | PhEmpty _ -> 0
    | Ph (_, sz, _, _) -> sz

let ph_isempty =
    function
    | PhEmpty _ -> true
    | _ -> false

let ph_find (Ph (_, _, e, _)) = e

let ph_merge =
    function
    | (x, PhEmpty _) -> x
    | (PhEmpty _, x) -> x
    | (((Ph (f, asz, a, xs)) as x), ((Ph (_, bsz, b, ys)) as y)) ->
        if f a b
            then Ph (f, asz + bsz, a, y :: xs)
            else Ph (f, asz + bsz, b, x :: ys)

let ph_ins =
    function
    | (PhEmpty f, e) -> ph_sing f e
    | ((Ph (f, _, _, _)) as x, e) -> ph_merge (x, ph_sing f e)

let rec ph_mp =
    function
    | (f, []) -> ph_empty f
    | (_, [x]) -> x
    | (f, x :: y :: rest) -> ph_merge (ph_merge (x, y), ph_mp (f, rest))

let ph_del (Ph (f, _, _, xs)) = ph_mp (f, xs)

let med_mk: (int pheap) * (int pheap) = (ph_empty (fun a b -> a > b), ph_empty (fun a b -> a < b))

let med_add (l, r) e =
    let (l0, r0) =
        if ph_isempty l || e <= ph_find l
            then (ph_ins (l, e), r)
            else (l, ph_ins (r, e))
    if ph_size l0 > 1 + (ph_size r0)
        then (ph_del l0, ph_ins (r0, ph_find l0))
        else
            if ph_size r0 > ph_size l0
                then (ph_ins (l0, ph_find r0), ph_del r0)
                else (l0, r0)

let med (l, _) = ph_find l

let rec tst n t (sq: ((int pheap) * (int pheap)) array) md =
    if n = t
        then ()
        else
            let x = int (Console.ReadLine ())
            let (l, r) as md0 =
                if x < 0
                    then sq.[n + x]
                    else med_add md x
            ignore (Console.WriteLine ("{0}", med md0))
            sq.[n] <- md0
            tst (n + 1) t sq md0

let main _ =
    let t = int (Console.ReadLine ())
    tst 0 t [|for i in 1 .. t -> med_mk|] med_mk

main ()
