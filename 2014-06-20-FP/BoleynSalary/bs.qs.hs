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

merge [] xs = xs
merge xs [] = xs
merge xs@(x : xs') ys@(y : ys') = if x <= y then x : merge xs' ys else y : merge xs ys'

arrlst xs = map (xs !) (U.indices xs)

main = do
    pstr <- getLine
    let (n : q : _) = map read (words pstr)
    es <- mapM (const readEdge) [1 .. pred n]
    let mchildren = Map.fromListWith (Seq.><) (map (\(a, b) -> (b, Seq.singleton a)) es)
    let children = buildArr listArray 1 n (\_ x -> Fld.toList $ if x `Map.member` mchildren then fromJust $ x `Map.lookup` mchildren else Seq.empty)
    let ranks = buildArr U.listArray 1 n (\mem x -> Fld.foldl' (+) 0 ((\x -> succ $ mem x) `map` (children ! x)))
    sstr <- getLine
    let sals = U.listArray (1 :: Int, n) (map (read :: String -> Int) (words sstr))
    tst q 0 children sals

--f x children sals = merge (sort $ (\x -> (sals ! x, x)) `map` (children ! x)) $ foldl' merge [] ((\x -> f x children sals) `map` (children ! x))
f x children sals = concat $ (sort $ (\x -> (sals ! x, x)) `map` (children ! x)) : (foldr (:) [] ((\x -> f x children sals) `map` (children ! x)))

qs k xs = if ll == k then pivot else if ll > k then qs k ls else qs (k - ll - 1) (tail rs)
    where
        pivot = head xs
        (ls, rs) = partition (< pivot) xs
        ll = length ls

g v k cs ss = qs k xs
    where
        xs = f v cs ss

tst 0 _ _ _ = return ()
tst q d children sals = do
    qstr <- getLine
    let (v : k : _) = map read (words qstr)
    --let d' =  snd $ (f (v + d) children sals) !! (pred k)
    let d' =  snd $ g (v + d) (pred k) children sals
    putStrLn $ show d'
    tst (pred q) d' children sals

readEdge :: IO (Int, Int)
readEdge = do
    estr <- getLine
    let (a : b : _) = map read (words estr)
    return (a, b)

