import Control.Monad
import Data.Array
import qualified Data.Foldable as Fld
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

(<|) = (Seq.<|)
(|>) = (Seq.|>)
(><) = (Seq.><)

data Ph a = EmptyPh | Ph a [Ph a] deriving Show

singPh x = Ph x []

minPh (Ph x _) = x

mergePh x EmptyPh = x
mergePh EmptyPh x = x
mergePh x@(Ph a as) y@(Ph b bs) =
    if a < b
        then Ph a (y : as)
        else Ph b (x : bs)

insertPh x e = x `mergePh` (singPh e)

delminPh (Ph _ xs) = mergePPh xs

mergePPh [] = EmptyPh
mergePPh [x] = x
mergePPh (x : y : rest) = (x `mergePh` y) `mergePh` (mergePPh rest)

findKPh 1 x = minPh x
findKPh k x = findKPh (pred k) (delminPh x)

readEdge :: IO (Int, Int)
readEdge = do
    estr <- getLine
    let (a : b : _) = map read (words estr)
    return (a, b)

calc n parents children sals = stats
    where
        stats = array (1, n) (prc 1)
        prc n =
            let chs = children ! n
                mine = foldl mergePh (foldl mergePh EmptyPh (map (\c -> singPh (sals ! c, c)) chs)) (map (stats !) chs)
                in
                    (chs >>= prc) ++ [(n, mine)]

tst 0 _ _ = return ()
tst q d' stats = do
    qstr <- getLine
    let (v : k : _) = map read (words qstr)
    let d = snd $ findKPh k (stats ! (v + d'))
    putStrLn (show d)
    tst (pred q) d stats
    
main = do
    pstr <- getLine
    let (n : q : _) = map read (words pstr)
    es <- mapM (const readEdge) [1 .. pred n]
    let parents = array (2, n) es
    let mchildren = Map.fromListWith (><) (map (\(a, b) -> (b, Seq.singleton a)) es)
    let children = array (1, n) (map (\x -> (x, Fld.toList $ if x `Map.member` mchildren then fromJust $ x `Map.lookup` mchildren else Seq.empty)) [1 .. n])
    sstr <- getLine
    let sals = array (1, n) (zip [1 ..] (map (read :: String -> Int) (words sstr)))
    let stats = calc n parents children sals
    tst q 0 stats

