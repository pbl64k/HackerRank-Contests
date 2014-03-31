import Data.List
import qualified Data.Map as M
import Data.Maybe

f1 fs = (>>= (\(p : ps) -> map (\f -> (f * p) : p : ps) fs))

f2 fs ps tgt =
    let ps' = filter ((<= tgt) . head) $ f1 fs ps
        ps'k = map (\x@(z : zs) -> (z, x)) ps'
        ps'map = M.fromListWith (\a b -> if (reverse a) < (reverse b) then a else b) ps'k
        in ps' : (f2 fs (map snd (M.toAscList ps'map)) tgt)

f tgt fs = f2 fs [[1]] tgt

rf fs tgt = (rf' . (f tgt)) fs tgt

rf' ([] : _) _ = []
rf' (bs@((x : _) : _) : bss) tgt =
    if x > tgt
        then []
        else
            let s = ff bs tgt
                in if s == Nothing
                    then rf' bss tgt
                    else fromJust s

ff bs tgt =
    let l = filter ((== tgt) . head) bs
        in if null l
            then Nothing
            else Just (head l)

explode = unfoldr f
    where f str = let (chunk, rest) = span (/= ' ') str
                   in if null chunk
                         then Nothing
                         else if null rest
                                 then Just (chunk, rest)
                                 else Just (chunk, tail rest)

main = do
    fl <- getLine
    let (nstr, _) = span (/= ' ') fl
    let n = read nstr
    astr <- getLine
    let as = filter ((== 0) . (n `mod`)) (sort $ map (read :: String -> Int) (explode astr))
    let ans = rf as n
    if null ans
        then putStrLn "-1"
        else (putStrLn . (intercalate " ") . (map show) . reverse) ans

