import Data.Array
import Data.Bits
import Data.List

uniq (a : xs@(b : _)) =
        if a == b
            then uniq xs
            else a : uniq xs
uniq xs = xs

mex = (mex' 0) . uniq . sort
    where
        mex' acc [] = acc
        mex' acc (x : xs) =
                if acc < x
                    then acc
                    else
                        if acc == x
                            then mex' (succ acc) xs
                            else mex' acc xs

nimber :: Array Int Int
nimber = array (0, 300) ((0, 0) : (map (\x -> (x, n x)) [1 .. 300]))
    where
        n = (mex . (map nimb) . posns)

posns x =
        [[x'] | dx <- [-1, -2], let x' = x + dx, x' >= 0] ++
        [[l, x - l - 1] | l <- [1 .. ((x - 1) `div` 2)]] ++
        [[l, x - l - 2] | l <- [1 .. ((x - 2) `div` 2)]]

nimb = (foldl1 xor) . (map (nimber !))

conv = conv' 0 []
    where
        conv' 0 acc [] = acc
        conv' n acc [] = n : acc
        conv' 0 acc ('X' : rst) = conv' 0 acc rst
        conv' n acc ('X' : rst) = n : (conv' 0 acc rst)
        conv' n acc ('I' : rst) = conv' (succ n) acc rst

out x =
        if x == 0
            then "LOSE"
            else "WIN"

tst = do
        getLine
        pos <- getLine
        let npos = conv pos
        (putStrLn . out . nimb) npos

main = do
        tstr <- getLine
        mapM (const tst) [1 .. (read tstr)]

