import Data.Array
import qualified Data.Array.Unboxed as U
import qualified Data.Foldable as Fld
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

buildArr la a b f = res
    where
        res = la (a, b) (map (f (res !)) [a .. b])

arrlst i j xs = map (xs !) [i .. j]

ordr cs acc n = Fld.foldr (\x acc -> ordr cs acc x) (n : acc) (cs ! n)

intersects (x1, x2) (y1, y2) = (x1 <= y1 && y1 <= x2) || (x1 <= y2 && y2 <= x2) || (y1 <= x1 && x1 <= y2)

data BinTree = BtEmpty Int | BtLeaf Int Int | BtNode Int Int BinTree BinTree deriving (Show, Eq, Ord)
data RangeTree = RtEmpty | RtLeaf Int BinTree | RtNode Int Int Int BinTree RangeTree RangeTree deriving (Show, Eq, Ord)

rtcons _ [] = (RtEmpty, (0, []))
rtcons _ [(x, y)] = (RtLeaf x (BtLeaf y 1), (1, [y]))
rtcons (l, r) xs = (RtNode m l r (fst $ btcons 0 bt) lrt rrt, bt)
    where
        m = (l + r) `div` 2
        (xl, xr) = part m xs
        (lrt, lbt) = rtcons (l, m) xl
        (rrt, rbt) = rtcons (succ m, r) xr
        bt = merge lbt rbt

css rt l r = css' rt l r []
    where
        css' RtEmpty _ _ acc = acc
        css' (RtLeaf x t) l r acc = if l <= x && x <= r then t : acc else acc
        css' (RtNode x l' r' t lt rt) l r acc =
            if r < l' || r' < l
                then acc
                else
                    if l <= l' && r' <= r
                        then t : acc
                        else css' lt l r (css' rt l r acc)

boundy rt xl xh yh k = bound (css rt xl xh) 0 yh k

bound ts yl yh k =
    if yl == yh
        then yh
        else
            if kym > k
                then bound ts yl (pred ym) k
                else
                    if kym < k
                        then bound ts (succ ym) yh k
                        else bound ts yl ym k
    where
        ym = (yl + yh) `div` 2
        kym = btsbound ts ym

btsbound ts x = sum $ map (\t -> btbound t x) ts

btcons p (0, xs) = (BtEmpty p, xs)
btcons p (1, (x : xs)) = (BtLeaf x (p + 1), xs)
btcons p (n, xs) = (BtNode x (p + m + 1) l r, xs'')
    where
        m = n `div` 2
        (l, (x : xs')) = btcons p (m, xs)
        (r, xs'') = btcons (p + m + 1) (n - m - 1, xs')

btbound (BtEmpty k) _ = k
btbound (BtLeaf x' k) x =
    if x' <= x
        then k
        else 0
btbound (BtNode x' k l r) x =
    if x == x'
        then k
        else
            if x < x'
                then btbound l x
                else max k $ btbound r x

merge (nl, xl) (nr, xr) = (nl + nr, merge' xl xr)
    where
        merge' xs [] = xs
        merge' [] ys = ys
        merge' xs@(x : xs') ys@(y : ys') =
            if x < y
                then x : merge' xs' ys
                else y : merge' xs ys'

part m xs = part' [] [] xs
    where
        part' l r [] = (l, r)
        part' l r (e@(x, y) : xs) =
            if x <= m
                then part' (e : l) r xs
                else part' l (e : r) xs
                       
main = do
    pstr <- getLine
    let (n : q : _) = map read (words pstr)
    es <- mapM (const readEdge) [1 .. pred n]
    let mchildren = Map.fromListWith (Seq.><) (map (\(a, b) -> (b, Seq.singleton a)) es)
    let children = buildArr listArray 1 n (\_ x -> Fld.toList $ if x `Map.member` mchildren then fromJust $ x `Map.lookup` mchildren else Seq.empty)
    let ranks = buildArr U.listArray 1 n (\mem x -> Fld.foldl' (+) 0 ((\x -> succ $ mem x) `map` (children ! x)))
    sstr <- getLine
    let sals = (map read $ words sstr) :: [Int]
    let ordering = ordr children [] 1
    let toord = (U.array (1, n) $ zip ordering [1 ..]) :: Array Int Int
    let fromord = (U.listArray (1, n) ordering) :: Array Int Int
    let ints = buildArr U.listArray 1 n (\mem x -> if null $ children ! (fromord ! x) then x else mem $ toord ! (head (children ! (fromord ! x))))
    let sorted = map snd (sort $ zip sals [1 .. n])
    let tosrt = (U.array (1, n) $ zip sorted [1 ..]) :: Array Int Int
    let fromsrt = (U.listArray (1, n) sorted) :: Array Int Int
    let ordsrt = (U.listArray (1, n) (((tosrt !) . (fromord !)) `map` [1 .. n])) :: Array Int Int
    let (rt, _) = rtcons (1, n) (map (\x -> (x, tosrt ! (fromord ! x))) [1 .. n])
    tst q 0 (toord, fromord, tosrt, fromsrt, ordsrt, ints, rt, n)

tst 0 _ _ = return ()
tst q d dt@(toord, fromord, tosrt, fromsrt, ordsrt, ints, t, n) = do
    qstr <- getLine
    let (v : k : _) = map read (words qstr)
    let vo = toord ! (v + d)
    let lo = ints ! vo
    let ro = pred vo
    let dord = boundy t lo ro n k
    let d' = fromsrt ! dord
    putStrLn $ show d'
    tst (pred q) d' dt

readEdge :: IO (Int, Int)
readEdge = do
    estr <- getLine
    let (a : b : _) = map read (words estr)
    return (a, b)

