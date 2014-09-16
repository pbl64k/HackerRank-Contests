ds = sum . ((read . (: [])) `map`)

f [x] = [x]
f s = f $ show $ ds s

main = do
    str <- getLine
    let (n : k : _) = words str
    putStrLn $ f $ show $ read k * ds n

