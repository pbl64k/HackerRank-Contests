import Data.Array
import qualified Data.Array.Unboxed as U
import qualified Data.Foldable as Fld
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

sqd xs = (x, xs')
    where
        (x Seq.:< xs') = Seq.viewl xs

data Sp a b = Sp a b deriving Show

spsnd (Sp _ x) = x

instance Eq a => Eq (Sp a b) where
    (Sp x _) == (Sp y _) = x == y

instance Ord a => Ord (Sp a b) where
    (Sp x _) `compare` (Sp y _) = x `compare` y

data Ph a = EmptyPh | Ph Int a [Ph a] deriving Show

sizePh EmptyPh = 0
sizePh (Ph x _ _) = x

singPh x = Ph 1 x []

minPh (Ph _ x _) = x

mergePh x EmptyPh = x
mergePh EmptyPh x = x
mergePh x@(Ph al a as) y@(Ph bl b bs) =
    if a < b
        then Ph (al + bl) a (y : as)
        else Ph (al + bl) b (x : bs)

insertPh x e = x `mergePh` (singPh e)

delminPh (Ph _ _ xs) = mergePPh xs

mergePPh [] = EmptyPh
mergePPh [x] = x
mergePPh (x : y : rest) = (x `mergePh` y) `mergePh` (mergePPh rest)

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

pqMerge :: Seq.Seq (Seq.Seq (Int, Int)) -> Seq.Seq (Int, Int)
pqMerge xs = pqMerge' $ Fld.foldl (\x e -> x `insertPh` (Sp (Seq.length e) e)) EmptyPh xs
    where
        pqMerge' xs =
            if sizePh xs == 1
                then spsnd $ minPh xs
                else
                    let (Sp _ seq1) = minPh xs
                        xs' = delminPh xs
                        (Sp _ seq2) = minPh xs'
                        xs'' = delminPh xs'
                        seq = seq1 `merge` seq2
                        in pqMerge' $ xs'' `insertPh` (Sp (Seq.length seq) seq)

m :: Ph (Sp (Int, Int) Int) -> Seq.Seq (Seq.Seq (Int, Int)) -> Seq.Seq (Int, Int)
m EmptyPh _ = Seq.empty
m ph@(Ph _ (Sp x ix) _) sq = x Seq.<|
    let ph' = delminPh ph
        in
            if Seq.null $ sq `Seq.index` ix
                then m ph' sq
                else
                    let (h, t) = sqd $ sq `Seq.index` ix
                        sq' = Seq.update ix t sq
                        in m (ph' `insertPh` (Sp h ix)) sq'

--pqMerge :: Seq.Seq (Seq.Seq (Int, Int)) -> Seq.Seq (Int, Int)
--pqMerge xs = m ph xs'
--    where
--        (ph, xs') = Seq.foldlWithIndex (\(ph, accsq) ix sq -> if Seq.null sq then (ph, accsq Seq.|> sq) else (ph `insertPh` (Sp (fst $ sqd sq) ix), accsq Seq.|> (snd $ sqd sq))) (EmptyPh, Seq.empty) xs

buildArr la a b f = res
    where
        res = la (a, b) (map (f (res !)) [a .. b])

main = do
    pstr <- getLine
    let (n : q : _) = map read (words pstr)
    es <- mapM (const readEdge) [1 .. pred n]
    let mchildren = Map.fromListWith (Seq.><) (map (\(a, b) -> (b, Seq.singleton a)) es)
    let children = buildArr listArray 1 n (\_ x -> if x `Map.member` mchildren then fromJust $ x `Map.lookup` mchildren else Seq.empty)
    let ranks = buildArr U.listArray 1 n (\mem x -> Fld.foldl' (+) 0 ((\x -> succ $ mem x) `fmap` (children ! x)))
    sstr <- getLine
    let sals = U.listArray (1 :: Int, n) (map (read :: String -> Int) (words sstr))
    let ks = buildArr listArray 1 n (\mem x -> pqMerge ((Seq.unstableSort $ (\x -> (sals ! x, x)) `fmap` (children ! x)) Seq.<| (mem `fmap` (children ! x))))
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

