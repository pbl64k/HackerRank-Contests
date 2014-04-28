main = getLine >> getLine >>= (putStrLn . show . (foldl1 lcm) . (map read) . words)
