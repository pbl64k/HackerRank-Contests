import Data.Array
import qualified Data.Array.Unboxed as U
import qualified Data.Foldable as Fld
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

data ITree = Leaf Int | Node Int Int Int ITree ITree deriving (Show, Eq, Ord)

buildArr la a b f = res
    where
        res = la (a, b) (map (f (res !)) [a .. b])

arrlst xs = map (xs !) (U.indices xs)

ordr cs acc n = Fld.foldr (\x acc -> ordr cs acc x) (n : acc) (cs ! n)

tsize (Leaf _) = 1
tsize (Node sz _ _ _ _) = sz

lint (Leaf l) = l
lint (Node _ l _ _ _) = l

rint (Leaf r) = r
rint (Node _ _ r _ _) = r

bit 1 (x : xs) = (Leaf x, xs)
bit n xs = (Node n (min (lint l) (lint r)) (max (rint l) (rint r)) l r, xs'')
    where
        midp = n `div` 2
        (l, xs') = bit midp xs
        (r, xs'') = bit (n - midp) xs'

intersects (x1, x2) (y1, y2) = (x1 <= y1 && y1 <= x2) || (x1 <= y2 && y2 <= x2) || (y1 <= x1 && x1 <= y2)

fnd (Leaf x) li ri 1 = if li <= x && x <= ri then x else 0
fnd (Leaf x) li ri _ = if li <= x && x <= ri then (-1) else 0
fnd (Node sz li ri lt rt) lo ro k =
    if lo <= li && ri <= ro && sz < k
        then (-sz)
        else
            if not $ intersects (li, ri) (lo, ro)
                then 0
                else
                    let n = fnd lt lo ro k
                        in
                            if n > 0
                                then n
                                else
                                    let n' = fnd rt lo ro (k + n)
                                        in
                                            if n' > 0
                                                then n'
                                                else n + n'

main = do
    pstr <- getLine
    let (n : q : _) = map read (words pstr)
    es <- mapM (const readEdge) [1 .. pred n]
    let mchildren = Map.fromListWith (Seq.><) (map (\(a, b) -> (b, Seq.singleton a)) es)
    let children = buildArr listArray 1 n (\_ x -> Fld.toList $ if x `Map.member` mchildren then fromJust $ x `Map.lookup` mchildren else Seq.empty)
    let ranks = buildArr U.listArray 1 n (\mem x -> Fld.foldl' (+) 0 ((\x -> succ $ mem x) `map` (children ! x)))
    sstr <- getLine
    --let sals = (U.listArray (1, n) (map read (words sstr))) :: Array Int Int
    let sals = (map read $ words sstr) :: [Int]
    let ordering = ordr children [] 1
    let toord = (U.array (1, n) $ zip ordering [1 ..]) :: Array Int Int
    let fromord = (U.listArray (1, n) ordering) :: Array Int Int
    let ints = buildArr U.listArray 1 n (\mem x -> if null $ children ! (fromord ! x) then x else mem $ toord ! (head (children ! (fromord ! x))))
    let sorted = sort $ zip sals (map (toord !) [1 .. n])
    let (intt, []) = bit n (map snd sorted)
    --putStrLn $ show sorted
    --putStrLn $ show intt
    tst q 0 toord fromord ints intt

tst 0 _ _ _ _ _ = return ()
tst q d toord fromord ints intt = do
    qstr <- getLine
    let (v : k : _) = map read (words qstr)
    let vo = toord ! (v + d)
    let lo = ints ! vo
    let ro = pred vo
    let dord = fnd intt lo ro k
    let d' = fromord ! dord
    putStrLn $ show d'
    tst (pred q) d' toord fromord ints intt

readEdge :: IO (Int, Int)
readEdge = do
    estr <- getLine
    let (a : b : _) = map read (words estr)
    return (a, b)

