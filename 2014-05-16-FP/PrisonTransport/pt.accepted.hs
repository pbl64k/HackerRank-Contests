import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

trav n em = foldl' (visit em) ([], S.empty) [1 .. n]

visit em s@(rs, vis) v =
        if v `S.member` vis
            then s
            else
                let (r', vis') = visit' em ([], vis) v
                    in (r' : rs, vis')

visit' em s@(acc, vis) v =
        if v `S.member` vis
            then s
            else foldl' (visit' em) (v : acc, v `S.insert` vis) (fromJust $ v `M.lookup` em)

ins (a, b) m = M.insert a (b : (fromJust $ a `M.lookup` m)) m

readEdge = do
        estr <- getLine
        let (a : b : _) = read `map` (words estr)
        return (a, b)

main = do
        nstr <- getLine
        let n = read nstr
        mstr <- getLine
        let m = read mstr
        es <- (const readEdge) `mapM` [1 .. m]
        let em0 = M.fromList $ ((flip (,)) []) `map` [1 .. n]
        let em = foldl' (\em (a, b) -> (a, b) `ins` (ins (b, a) em)) em0 es
        let cs = trav n em
        let rs = (ceiling . sqrt . fromIntegral . length) `map` (fst cs)
        (putStrLn . show . sum) rs

