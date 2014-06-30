import Data.Array
import qualified Data.Array.Unboxed as U
import qualified Data.Foldable as Fld
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

import System.Random
import Data.Array.IO
import Control.Monad

import System.IO.Unsafe

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs = newListArray (1, n) xs

buildArr la a b f = res
    where
        res = la (a, b) (map (f (res !)) [a .. b])

arrlst i j xs = map (xs !) [i .. j]

ordr cs acc n = Fld.foldr (\x acc -> ordr cs acc x) (n : acc) (cs ! n)

intersects (x1, x2) (y1, y2) = (x1 <= y1 && y1 <= x2) || (x1 <= y2 && y2 <= x2) || (y1 <= x1 && x1 <= y2)

data KdTree = Leaf | Node Bool Int Int Int Int Int Int Int KdTree KdTree deriving (Show, Eq, Ord)

part flag pivot xs = part' (not flag, 0, Nothing, []) (not flag, 0, Nothing, []) xs
    where
        f = if flag then snd else fst
        pc = f pivot
        pf c = f c < pc
        add (flag, 0, Nothing, []) e@(x, y) = (flag, 1, Just (x, x, y, y), [e])
        add (flag, n, Just (xl, xh, yl, yh), xs) e@(x, y) = (flag, n + 1, Just (min xl x, max xh x, min yl y, max yh y), e : xs)
        part' accl accr [] = (accl, accr)
        part' accl accr (x : xs) = if pf x then part' (add accl x) accr xs else part' accl (add accr x) xs

cons (_, 0, Nothing, []) = Leaf
cons (flag, n, Just (xl, xh, yl, yh), xs) = Node flag (n - 1) x y xl xh yl yh (cons l) (cons r)
    where
        -- cannot change pivot easily
        pivot@(x, y) = head xs
        (l, r) = part flag pivot (tail xs)

cnt Leaf _ _ _ _ = 0
cnt (Node _ n x y xl' xh' yl' yh' l r) xl xh yl yh =
    --(unsafePerformIO $ putStrLn $ show (n, xl, xh, yl, yh, xl', xh', yl', yh')) `seq`
    if not (intersects (xl, xh) (xl', xh') && intersects (yl, yh) (yl', yh'))
        then 0
        else
            (if xl <= x && x <= xh && yl <= y && y <= yh then 1 else 0) +
            (if xl <= xl' && xh' <= xh && yl <= yl' && yh' <= yh
                then n
                else cnt l xl xh yl yh + cnt r xl xh yl yh)

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
                        --then boundy t xl xh (succ ym) yh k
                        else boundy t xl xh yl ym k
    where
        ym = (yl + yh) `div` 2
        kym = cnt t xl xh yl ym
        --kym = cnt t xl xh 0 ym

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
    --let qt = cons (n, Just (0, n, 0, n), (zip (arrlst 1 n toord) (arrlst 1 n tosrt)))
    xxs <- shuffle (zip (arrlst 1 n toord) (arrlst 1 n tosrt))
    let qt = cons (True, n, Just (0, n, 0, n), xxs)
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

