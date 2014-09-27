f _ n [] = if n > 1 then show n else []
f c n (x : xs) =
    if c == x
        then f c (succ n) xs
        else
            (if n > 1 then show n else []) ++ (x : (f x 1 xs))

main = do
    s <- getLine
    putStrLn $ f ' ' 0 s

