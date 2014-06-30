import Data.List

conv m xs = map (\(w, k) -> w + (m - 1) * k) xs

sat mx m xs = sum (take m xs') <= mx
    where
        xs' = sort $ conv m xs
        
slv mx a b xs =
    if a == b
        then a
        else
            let m = ((a + b) + 1) `div` 2
                in
                    if sat mx m xs
                        then slv mx m b xs
                        else slv mx a (pred m) xs
main = do
    fstr <- getLine
    let (tot : m : _) = map (read :: String -> Int) (words fstr)
    astr <- getLine
    let as = map (read :: String -> Int) (words astr)
    hstr <- getLine
    let hs = map (read :: String -> Int) (words hstr)
    let xs = sortBy (\(a, x) (b, y) -> (x, a) `compare` (y, b)) (zip as hs)
    putStrLn (show $ slv m 0 tot xs)

