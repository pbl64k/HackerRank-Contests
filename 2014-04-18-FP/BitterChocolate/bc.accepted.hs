import Data.Array
import Data.List

limit = 25

lose = array ((0, 0, 0), (limit, limit, limit)) ls
        where
            check (c, b, a) = not $ any (lose !) (concat [
                    [(x, b, a) | x <- [0 .. c - 1]],
                    [(min c x, x, a)| x <- [0 .. b - 1]],
                    [(min c x, min b x, x) | x <- [0 .. a - 1]]])
            lose' (0, 0, 0) = False
            lose' (0, 0, 1) = True
            lose' ix@(c, b, a) = b > a || c > b || check ix
            lens = [0 .. limit]
            ls = [(ix, lose' ix) | a <- lens, b <- lens, c <- lens, let ix = (c, b, a)]

tst = do
        rstr <- getLine
        let (a:b:c:_) = map read (words rstr)
        putStrLn (if lose ! (c, b, a) then "LOSE" else "WIN")

main = do
        tstr <- getLine
        mapM_ (const tst) [1 .. (read tstr)]

