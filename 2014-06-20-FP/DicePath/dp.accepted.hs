import Data.Array
import qualified Data.Map as M

sz = 60

data Die = D Int Int Int Int Int Int deriving (Show, Eq, Ord)

right (D t f l k r b) = (D l f b k t r)

down (D t f l k r b) = (D k t l b r f)

initl = (D 1 2 3 5 4 6)

val (D t _ _ _ _ _) = t

dp = array ((1, 1), (60, 60)) (((1, 1), (M.fromList $ [(initl, 1)])) : lst)
    where
        move f (pos, v) = let pos' = f pos in (pos', v + val pos')
        left x y = if x > 1 then map (move right) (M.toList $ dp ! (x - 1, y)) else []
        up x y = if y > 1 then map (move down) (M.toList $ dp ! (x, y - 1)) else []
        f x y = M.fromListWith max (left x y ++ up x y)
        lst = [((x, y), f x y) | d <- [2 .. sz * 2], x <- [(max 1 (d - sz + 1)) .. (min d sz)], let y = d + 1 - x]

dpmax x y = maximum (map snd $ M.toList (dp ! (y, x)))

tst = do
    cstr <- getLine
    let (x : y : _) = map read (words cstr)
    putStrLn (show $ dpmax x y)

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. (read tstr)]

