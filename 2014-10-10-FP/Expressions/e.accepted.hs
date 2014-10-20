import qualified Data.IntMap as M

mmm = 101

upd n (r, str) =
    [((r + n) `mod` mmm, str ++ "+" ++ show n),
    ((r - n) `mod` mmm, str ++ "-" ++ show n),
    ((r * n) `mod` mmm, str ++ "*" ++ show n)]

slv acc [] = acc
slv acc (n : ns) = slv acc' ns
    where
        acc' = M.fromList (M.toList acc >>= upd n)

main = do
    getLine
    nstr <- getLine
    let (n : ns) = read `map` (words nstr)
    putStrLn $ (slv (M.singleton n (show n)) ns) M.! 0

