import Data.List

main = getLine >>= (putStrLn . show . (\(n : m : _) -> (foldl' (((`mod` 1000000007) .) . (*)) 1 [gcd x y | x <- [1 .. n], y <- [1 .. m]])) . (map read) . words)
