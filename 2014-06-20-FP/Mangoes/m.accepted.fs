open System

let readIntList _ =
    let str = Console.ReadLine ()
    List.map (fun x -> int64 x) (List.ofSeq (str.Split [|' '|]))

let rec take n xs =
    if n = 0L || (List.isEmpty xs)
        then []
        else (List.head xs) :: (take (n - 1L) (List.tail xs))

let sat mx m xs =
    List.sum (take m (List.sort (List.map (fun (w, k) -> w + (m - 1L) * k) xs))) <= mx

let rec solve mx a b xs =
    if a = b
        then a
        else
            let m = (a + b + 1L) / 2L
            if sat mx m xs
                then solve mx m b xs
                else solve mx a (m - 1L) xs

let main _ =
    let ps = readIntList ()
    let ys = readIntList ()
    let hs = readIntList ()
    let xs = List.sortWith (fun (a, x) (b, y) -> compare x y) (List.zip ys hs)
    Console.WriteLine("{0}", solve (ps.Item(1)) 0L (ps.Item(0)) xs)

main ()
