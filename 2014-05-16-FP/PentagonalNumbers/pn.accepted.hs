main = getLine >>= (\t -> mapM (const $ (getLine >>= (putStrLn . show . (\x -> (3 * x ^ 2 - x) `div` 2) . read))) [1 .. (read t)])
