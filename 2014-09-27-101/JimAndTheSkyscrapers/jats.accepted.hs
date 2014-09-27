import qualified Data.IntMap as IM
import Data.Maybe

solve r _ [] = r
solve r t (h : hs) = solve (r + (2 * r')) (IM.insert h (succ r') t') hs
    where
        (_, n, t') = IM.splitLookup h t
        r' = fromMaybe 0 n
        
main = do
    getLine
    hstr <- getLine
    let hs = read `map` (words hstr)
    putStrLn $ show $ solve 0 IM.empty hs

