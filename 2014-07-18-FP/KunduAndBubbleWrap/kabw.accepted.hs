import Data.Ratio

f n = sum $ ((fromIntegral n) /) `map` [1.0 .. fromIntegral n]

gamma = 57721566490153286060651209008240243104215933593992 % 100000000000000000000000000000000000000000000000000

g n = (n % 1) * (gamma + (toRational $ log (fromIntegral n)))

main = do
    str <- getLine
    let (n : m : _) = read `map` (words str)
    putStrLn $ show (f (n * m))

