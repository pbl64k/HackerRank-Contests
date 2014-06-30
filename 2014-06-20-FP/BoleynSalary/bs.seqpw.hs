import Data.Array
import qualified Data.Array.Unboxed as U
import qualified Data.Foldable as Fld
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

buildArr la a b f = res
    where
        res = la (a, b) (map (f (res !)) [a .. b])

merge :: Seq.Seq (Int, Int) -> Seq.Seq (Int, Int) -> Seq.Seq (Int, Int)
merge xs ys =
    if Seq.null xs
        then ys
        else
            if Seq.null ys
                then xs
                else
                    if x < y
                        then
                            let (l, r) = Seq.spanl (< y) xs
                                in
                                    l Seq.>< merge r ys
                        else
                            let (l, r) = Seq.spanl (< x) ys
                                in
                                    l Seq.>< merge xs r
    where
        (x Seq.:< xs') = Seq.viewl xs
        (y Seq.:< ys') = Seq.viewl ys

arrlst xs = map (xs !) (U.indices xs)

main = do
    pstr <- getLine
    let (n : q : _) = map read (words pstr)
    es <- mapM (const readEdge) [1 .. pred n]
    let mchildren = Map.fromListWith (Seq.><) (map (\(a, b) -> (b, Seq.singleton a)) es)
    let children = buildArr listArray 1 n (\_ x -> if x `Map.member` mchildren then fromJust $ x `Map.lookup` mchildren else Seq.empty)
    let ranks = buildArr U.listArray 1 n (\mem x -> Fld.foldl' (+) 0 ((\x -> succ $ mem x) `fmap` (children ! x)))
    sstr <- getLine
    let sals = U.listArray (1 :: Int, n) (map (read :: String -> Int) (words sstr))
    let ks = buildArr listArray 1 n (\mem x -> merge (Seq.unstableSort $ (\x -> (sals ! x, x)) `fmap` (children ! x)) $ Fld.foldl' merge Seq.empty (mem `fmap` (children ! x)))
    tst q 0 ks

tst 0 _ _ = return ()
tst q d ks = do
    qstr <- getLine
    let (v : k : _) = map read (words qstr)
    let d' = snd $ (ks ! (v + d)) `Seq.index` (pred k)
    putStrLn $ show d'
    tst (pred q) d' ks

readEdge :: IO (Int, Int)
readEdge = do
    estr <- getLine
    let (a : b : _) = map read (words estr)
    return (a, b)

