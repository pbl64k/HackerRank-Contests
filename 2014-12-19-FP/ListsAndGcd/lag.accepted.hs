import Control.Monad
import Data.List

readlst = map (read :: String -> Integer) . words

pairs [] = []
pairs (x : y : xs) = (x, y) : pairs xs

gcd' [] _ = []
gcd' _ [] = []
gcd' xs@((x, n) : xs') ys@((y, m) : ys') =
    if x < y
        then gcd' xs' ys
        else
            if y < x
                then gcd' xs ys'
                else (x, min n m) : gcd' xs' ys'

f st _ = do
    xstr <- getLine
    let x = pairs $ readlst xstr
    return $ gcd' st x
    
dsp (x, y) = show x ++ " " ++ show y

main = do
    nstr <- getLine
    let n = read nstr
    initstr <- getLine
    let init = pairs $ readlst initstr
    res <- foldM f init [2 .. n]
    putStrLn $ intercalate " " $ dsp `map` res

