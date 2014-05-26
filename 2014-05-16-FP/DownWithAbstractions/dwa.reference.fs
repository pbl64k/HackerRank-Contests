open System

type Lam = Var of String | App of (Lam * Lam) | Lam of (String * Lam)

let rec parseExpr cs =
        match cs with
        | '(' :: '\\' :: rest -> parseLam rest
        | '(' :: rest -> parseApp rest
        | _ -> parseVar cs
and parseVar cs =
        let rec aux cs acc =
                match cs with
                | [] -> (acc, cs)
                | c :: rest ->
                        if c >= 'a' && c <= 'z' ||
                                c >= 'A' && c <= 'Z' ||
                                c >= '0' && c <= '9' ||
                                c = '_'
                            then aux rest (c :: acc)
                            else (acc, cs)
        let (n, rest) = aux cs []
        (Var (System.String (List.toArray (List.rev n))), rest)
and parseApp cs =
        let (e1, ' ' :: rest1) = parseExpr cs
        let (e2, ')' :: rest2) = parseExpr rest1
        (App (e1, e2), rest2)
and parseLam cs =
        let (Var n, c :: rest1) = parseVar cs
        if c = '.'
            then
                let (e, ')' :: rest2) = parseExpr (List.tail rest1)
                (Lam (n, e), rest2)
            else
                let (e, rest2) = parseLam rest1
                (Lam (n, e), rest2)

let parse x =
        let (expr, _) = parseExpr x
        expr

let rec free x expr =
        match expr with
        | Var z -> x = z
        | App (e1, e2) -> free x e1 || free x e2
        | Lam (n, e) -> x <> n && free x e

let rec t x =
        match x with
        | (Var _) as z -> z
        | App (e1, e2) -> App (t e1, t e2)
        | Lam (n1, (Var n2 as e)) ->
                if n1 = n2
                    then Var "I"
                    else App (Var "K", e)
        | Lam (n1, (Lam _ as e1)) ->
                if free n1 e1
                    then t (Lam (n1, t e1))
                    else App (Var "K", t e1)
        | Lam (n, (App (e1, e2) as e)) ->
                let tt _ =
                        match (free n e1, free n e2) with
                        | (true, true) -> App (App (Var "S", t (Lam (n, e1))), t (Lam (n, e2)))
                        | (true, false) -> App (App (Var "C", t (Lam (n, e1))), t e2)
                        | (false, true) -> App (App (Var "B", t e1), t (Lam (n, e2)))
                        | _ -> App (Var "K", t e)
                match e2 with
                | Var x ->
                        if n = x && not (free n e1)
                            then t e1
                            else tt ()
                | _ -> tt ()

let rec neat x =
        match x with
        | Var x -> x
        | App (e1, (App (_, _) as e2)) -> neat e1 + "(" + neat e2 + ")"
        | App (e1, e2) -> neat e1 + neat e2
        | Lam _ -> "FAIL"

let tst _ =
        let x = Console.ReadLine ()
        Console.WriteLine ("{0}", neat (t (parse (List.ofSeq x))))

let main _ =
        let t = int (Console.ReadLine ())
        ignore (List.map tst [1 .. t])

main ()
