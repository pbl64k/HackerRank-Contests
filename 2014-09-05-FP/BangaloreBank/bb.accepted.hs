import qualified Data.Map as M

solve st [] = st
solve st (x : xs) = solve newst xs
    where
        flatst = M.toList st
        flatst' = [r | ((a, b), v) <- flatst, r <- [((x, b), v + (abs $ a - x) + 1), ((a, x), v + (abs $ b - x) + 1)]]
        newst = M.fromListWith min flatst'

slv (x : xs) = minimum $ M.elems res
    where
        init = M.fromList [((x, z), 1) | z <- [1 .. 10]]
        res = solve init xs

main = do
    getLine
    dstr <- getLine
    let digits = ((\x -> if x == 0 then 10 else x) . read) `map` (words dstr)
    putStrLn $ show $ slv digits

