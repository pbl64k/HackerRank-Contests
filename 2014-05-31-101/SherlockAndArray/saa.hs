f (x : xs) = f' 0 (sum xs) x xs
    where
        f' a b x [] = if a == b then "YES" else "NO"
        f' a b x (x' : xs) = if a == b then "YES" else f' (a + x) (b - x') x' xs
    
tst = do
    getLine
    xstr <- getLine
    let xs = map read (words xstr)
    putStrLn (f xs)

main = do
    tstr <- getLine
    mapM (const tst) [1 .. (read tstr)]

