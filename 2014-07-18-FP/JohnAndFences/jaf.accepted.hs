import qualified Data.Set as IS
import Data.List
import Data.Maybe

fm def x = maybe def id x

best _ acc _ _ _ [] = acc
best mx acc last setlast set ((sz, pos) : rest) =
    if sz == last
        then best mx (max acc v) sz setlast set' rest
        else best mx (max acc v) sz set' set' rest
    where
        set' = pos `IS.insert` set
        s = if sz == last then setlast else set
        a = (fm 0 $ pos `IS.lookupLT` s) + 1
        b = (fm (succ mx) $ pos `IS.lookupGT` s) - 1
        v = (b - a + 1) * sz

solve :: [Int] -> Int
solve xs = best (length xsp) 0 (-1) IS.empty IS.empty xsp
    where
        xsp = sort $ zip xs [1 ..]

main = do
    getLine
    str <- getLine
    let xs = read `map` (words str)
    putStrLn $ show (solve xs)

