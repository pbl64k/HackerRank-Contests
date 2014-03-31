import qualified Data.Set

data Re = Nil | Eps | Any | Lit Char | Choice [Re] | Seq Re Re | Kleene Re deriving (Eq, Ord, Show)

match re [] = isEps re
match re (c : cs) =
    if isEps re
        then True
        else match (bdu c re) cs

bdu c re = unify $ bd c re

unify (Choice res) =
    let l = (Data.Set.toList . Data.Set.fromList) (filter (not . isNil) (map unify res))
        in
            if null l
                then Nil
                else
                    if null (tail l)
                        then head l
                        else Choice l
unify (Seq Eps re2) = unify re2
unify (Seq re1 re2) = Seq (unify re1) (unify re2)
unify (Kleene re) = Kleene (unify re)
unify x = x

bd _ Nil = Nil
bd _ Eps = Nil
bd _ Any = Eps
bd c (Lit cc) = if c == cc then Eps else Nil
bd c (Choice res) = Choice (map (bd c) res)
bd c (Seq re1 re2) =
    let b = Seq (bd c re1) re2
        in
            if isEps re1
                then Choice [b, (bd c re2)]
                else b
bd c (Kleene re) = Seq (bd c re) (Kleene re)

isEps Eps = True
isEps (Choice res) = any isEps res
isEps (Seq re1 re2) = (isEps re1) && (isEps re2)
isEps (Kleene _) = True
isEps _ = False

isNil Nil = True
isNil (Choice res) = all isNil res
isNil (Seq re1 re2) = (isNil re1) || (isNil re2)
isNil _ = False

re (c : []) = Lit c
re (c : cs) = Seq (Lit c) (re cs)

tst = do
    haystack <- getLine
    needle <- getLine
    putStrLn (if match (Seq (Kleene Any) (re needle)) haystack then "YES" else "NO")

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. (read tstr)]

