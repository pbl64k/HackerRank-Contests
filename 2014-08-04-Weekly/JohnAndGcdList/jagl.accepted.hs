import Data.List

tst = do
    getLine
    xstr <- getLine
    let xs = map read (words xstr)
    putStrLn $ intercalate " " $ map show $ map (uncurry lcm) (zip (1 : xs) (xs ++ [1]))

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. read tstr]

