f :: Integer -> Integer -> Integer -> Integer
f acc x n =
    if x `mod` n == 0
        then f (succ acc) (x `div` n) n
        else acc

cd = product . (cd' [] 2)
    where
        cd' acc n x =
            if n > x
                then acc
                else
                    if x `mod` n == 0
                        then
                            let p = f 1 x n
                                in cd' (p : acc) (succ n) (x `div` (n ^ (p - 1)))
                        else cd' acc (succ n) x

tst = do
    s <- getLine
    let (astr, bstr) = span (/= ' ') s
    let a = read astr
    let b = read $ tail bstr
    (putStrLn . show . cd) (gcd a b)

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. (read tstr)]

