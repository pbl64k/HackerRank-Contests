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
    --let klst = buildArr 1 n (\mem x -> merge (sort $ (\x -> (sals ! x, x)) `map` (children ! x)) $ Fld.foldl' merge [] (mem `map` (children ! x)))
    --let ks = buildArr 1 n (\_ x -> array (1, (ranks ! x)) (zip [1 ..] (klst ! x)))
    --let ks = buildArr listArray 1 n (\mem x -> U.listArray (1, ranks ! x) $ merge (sort $ (\x -> (sals ! x, x)) `map` (children ! x)) $ foldl' merge [] ((arrlst . mem) `map` (children ! x)))
    tst q 0 children sals

f x children sals = merge (sort $ (\x -> (sals ! x, x)) `map` (children ! x)) $ foldl' merge [] ((\x -> f x children sals) `map` (children ! x))

tst 0 _ _ _ = return ()
tst q d children sals = do
    qstr <- getLine
    let (v : k : _) = map read (words qstr)
    --let d' = snd $ (ks ! (v + d)) !! (pred k)
    let d' =  snd $ (f (v + d) children sals) !! (pred k)
    putStrLn $ show d'
    --tst (pred q) d' ks
    tst (pred q) d' children sals

readEdge :: IO (Int, Int)
readEdge = do
    estr <- getLine
    let (a : b : _) = map read (words estr)
    return (a, b)

