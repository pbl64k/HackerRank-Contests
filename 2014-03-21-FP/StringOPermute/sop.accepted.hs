perm [] = []
perm (a : b : xs) = b : a : (perm xs)

tst = getLine >>= (putStrLn . perm)

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. (read tstr)]

