g = foldl1 gcd

tst = do
    getLine
    xstr <- getLine
    let xs = map read (words xstr)
    putStrLn $ if g xs == 1 then "YES" else "NO"

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. (read tstr)]

