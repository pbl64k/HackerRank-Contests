import Data.Array
import qualified Data.Array.Unboxed as U
import qualified Data.Foldable as Fld
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

import System.IO.Unsafe

buildArr la a b f = res
    where
        res = la (a, b) (map (f (res !)) [a .. b])

arrlst i j xs = map (xs !) [i .. j]

ordr cs acc n = Fld.foldr (\x acc -> ordr cs acc x) (n : acc) (cs ! n)

intersects (x1, x2) (y1, y2) = (x1 <= y1 && y1 <= x2) || (x1 <= y2 && y2 <= x2) || (y1 <= x1 && x1 <= y2)

data QTree = Empty | Leaf Int Int | Node Int Int Int Int Int QTree QTree QTree QTree deriving (Show, Eq, Ord)

part xm ym xs = part' (0, Nothing, []) (0, Nothing, []) (0, Nothing, []) (0, Nothing, []) xs
    where
        add (0, Nothing, []) (x, y) = (1, Just (x, x, y, y), [(x, y)])
        add (n, Just (xl, xh, yl, yh), xs) (x, y) = (succ n, Just (min x xl, max x xh, min y yl, max y yh), (x, y) : xs)
        part' xlyl xlyh xhyl xhyh [] = (xlyl, xlyh, xhyl, xhyh)
        part' xlyl xlyh xhyl xhyh ((x, y) : xs) =
            if x < xm && y < ym
                then part' (add xlyl (x, y)) xlyh xhyl xhyh xs
                else if x < xm && y >= ym
                    then part' xlyl (add xlyh (x, y)) xhyl xhyh xs
                    else if x >= xm && y < ym
                        then part' xlyl xlyh (add xhyl (x, y)) xhyh xs
                        else part' xlyl xlyh xhyl (add xhyh (x, y)) xs

cons (_, _, []) = Empty
cons (_, _, [(x, y)]) = Leaf x y
cons (n, Just (xl, xh, yl, yh), xs) = Node n xl xh yl yh txlyl txlyh txhyl txhyh
    where
        xm = (xl + xh + 1) `div` 2
        ym = (yl + yh + 1) `div` 2
        --(xlyl, xs') = partition (\(x, y) -> x < xm && y < ym) xs
        --(xlyh, xs'') = partition (\(x, y) -> x < xm && y >= ym) xs'
        --(xhyl, xs''') = partition (\(x, y) -> x >= xm && y < ym) xs''
        --xhyh = xs'''
        --txlyl = cons xlyl xl (pred xm) yl (pred ym)
        --txlyh = cons xlyh xl (pred xm) ym yh
        --txhyl = cons xhyl xm xh yl (pred ym)
        --txhyh = cons xhyh xm xh ym yh
        (xlyl, xlyh, xhyl, xhyh) = part xm ym xs
        txlyl = cons xlyl
        txlyh = cons xlyh
        txhyl = cons xhyl
        txhyh = cons xhyh

cnt Empty _ _ _ _ = 0
cnt (Leaf x y) xl xh yl yh = if xl <= x && x <= xh && yl <= y && y <= yh then 1 else 0
cnt (Node n xl' xh' yl' yh' xlyl xlyh xhyl xhyh) xl xh yl yh =
    --(unsafePerformIO $ putStrLn $ show ('!', (xl, xh), (xl', xh'), (yl, yh), (yl', yh'), not (intersects (xl, xh) (xl', xh') && intersects (yl, yh) (yl', yh')))) `seq`
    if xl <= xl' && xh' <= xh && yl <= yl' && yh' <= yh
        then n
        else
            if not (intersects (xl, xh) (xl', xh') && intersects (yl, yh) (yl', yh'))
                then 0 --(unsafePerformIO $ putStrLn $ "ALARM!") `seq` 0
                else sum $ map (\t -> cnt t xl xh yl yh) [xlyl, xlyh, xhyl, xhyh]

--boundy t xl xh yl yh k =
--    if kym > k
--        then boundy t xl xh yl (pred ym) k
--        else
--            if kym < k
--                then boundy t xl xh ym yh k
--                else -- kym == k
--                    if yl + 1 == ym
--                        then ym
--                        else boundy t xl xh yl ym k
--    where
--        ym = (yl + yh + 1) `div` 2
--        kym = cnt t xl xh 0 ym

boundy t xl xh yl yh k =
    --(unsafePerformIO $ putStrLn $ show (xl, xh, yl, yh, ym, kym, k)) `seq`
    if yl == yh
        then yh
        else
            if kym > k
                then boundy t xl xh yl (pred ym) k
                else
                    if kym < k
                        then boundy t xl xh (succ ym) yh (k - kym)
                        else boundy t xl xh yl ym k
    where
        ym = (yl + yh) `div` 2
        kym = cnt t xl xh yl ym

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
    let sorted = map snd (sort $ zip sals [1 .. n])
    let tosrt = (U.array (1, n) $ zip sorted [1 ..]) :: Array Int Int
    let fromsrt = (U.listArray (1, n) sorted) :: Array Int Int
    let ordsrt = (U.listArray (1, n) (((tosrt !) . (fromord !)) `map` [1 .. n])) :: Array Int Int
    let qt = cons (n, Just (0, n, 0, n), (zip (arrlst 1 n toord) (arrlst 1 n tosrt)))
    --putStrLn $ show qt
    tst q 0 (toord, fromord, tosrt, fromsrt, ordsrt, ints, qt, n)

tst 0 _ _ = return ()
tst q d dt@(toord, fromord, tosrt, fromsrt, ordsrt, ints, qt, n) = do
    qstr <- getLine
    let (v : k : _) = map read (words qstr)
    let vo = toord ! (v + d)
    let lo = ints ! vo
    let ro = pred vo
    --putStrLn $ show (':', v, k, d, vo, lo, ro)
    --putStrLn $ show (cnt qt lo ro 0 15000)
    --putStrLn $ show (cnt qt lo ro 15001 26250)
    --putStrLn $ show (cnt qt lo ro 0 26250)
    --let dord = qs (pred k) (arrlst lo ro ordsrt)
    let dord = boundy qt lo ro 0 n k
    let d' = fromsrt ! dord
    putStrLn $ show d'
    tst (pred q) d' dt

readEdge :: IO (Int, Int)
readEdge = do
    estr <- getLine
    let (a : b : _) = map read (words estr)
    return (a, b)

