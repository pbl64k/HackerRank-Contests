import Control.Monad
import Data.List
import qualified Data.Map as M

match1 ws ([], acc) c = ws >>= (\w -> match1 ws (w, ' ':acc) c)
match1 ws (c':cs, acc) c =
        if c' == c
            then [(cs, c:acc)]
            else []

uniquify = M.toAscList . M.fromList

match ws = foldl (\ps c -> uniquify $ ps >>= (\pr -> match1 ws pr c)) [([], [])]

fm [] = "WRONG PASSWORD"
fm (([], m):_) = (tail . reverse) m
fm (_:ms) = fm ms

match' = (fm .) . match

tst = do
        getLine
        str <- getLine
        p <- getLine
        putStrLn (match' (words str) p)

main = do
        tstr <- getLine
        mapM_ (const tst) [1 .. (read tstr)]

