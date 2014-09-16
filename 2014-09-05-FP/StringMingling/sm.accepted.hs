main = do
    a <- getLine
    b <- getLine
    let f (a, b) = [a, b]
    putStrLn $ (a `zip` b) >>= f
