import Data.List

slv _ maxn _ [] = maxn
slv tot maxn st@(n, mult, rem) ((wt, mt) : rest) = if deadend then maxn else maxn''
    where
        d = (2 * mult + 2 * mt * n - mt) ^ 2 - 8 * mt * (wt - rem)
        mx = if d < 0 then tot else floor $ ((fromIntegral $ mt - 2 * mult - 2 * mt * n) + (sqrt $ fromIntegral d)) / (fromIntegral $ 2 * mt)
        deadend = n + (min tot mx) <= maxn
        maxn' =
            if wt + mult + (mt * n) <= rem
                then slv (pred tot) (max maxn (succ n)) (succ n, mult + mt, rem - (wt + mult + (mt * n))) rest
                else maxn
        maxn'' = slv (pred tot) maxn' st rest

main = do
    fstr <- getLine
    let (tot : m : _) = map (read :: String -> Int) (words fstr)
    astr <- getLine
    let as = map (read :: String -> Int) (words astr)
    hstr <- getLine
    let hs = map (read :: String -> Int) (words hstr)
    let xs = sortBy (\(_, x) (_, y) -> x `compare` y) (zip as hs)
    putStrLn (show $ slv tot 0 (0, 0, m) xs)

