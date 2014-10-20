open System

let rec slv =
    function
    | (pfx, [], y) -> (pfx, [], y)
    | (pfx, x, []) -> (pfx, x, [])
    | (pfx, ((x :: xs') as xs), ((y :: ys') as ys)) ->
        if x = y
            then slv (x :: pfx, xs', ys')
            else (pfx, xs, ys)

let tostr x = System.String (List.toArray x)

let main _ =
    let astr = List.ofSeq (Console.ReadLine ())
    let bstr = List.ofSeq (Console.ReadLine ())
    let (a, b, c) = slv ([], astr, bstr)
    Console.WriteLine ("{0} {1}", a.Length, tostr (List.rev a))
    Console.WriteLine ("{0} {1}", b.Length, tostr b)
    Console.WriteLine ("{0} {1}", c.Length, tostr c)

main ()

