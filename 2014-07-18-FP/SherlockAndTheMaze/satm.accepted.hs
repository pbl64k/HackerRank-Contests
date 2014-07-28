import Data.Array
import Data.Int

mm = 1000000007

dp :: Array (Int, Int, Int) Int64
dp = array ((1, 1, 0), (100, 100, 100)) lst
    where
        g n m k = if n > 0 && m > 0 && k >= 0 then dp ! (n, m, k) else 0
        f _ 1 0 = 1
        f 1 1 _ = 0
        f _ 1 _ = 0
        f 1 _ _ = 0
        f n m k = ((g (n - 1) m k) + (g m (n - 1) (k - 1))) `mod` mm
        lst = [((n, m, k), f n m k) | n <- [1 .. 100], m <- [1 .. 100], k <- [0 .. 100]]

tst = do
    tstr <- getLine
    let (n : m : k : _) = read `map` (words tstr)
    if n == 1 && m == 1
        then putStrLn "1"
        else putStrLn $ show (foldl (\a b -> (a + b) `mod` mm) 0 $ (\x -> (dp ! (n, m, x)) + (dp ! (m, n, x))) `map` [0 .. k])

main = do
    tstr <- getLine
    (const tst) `mapM_` [1 .. (read tstr)]

