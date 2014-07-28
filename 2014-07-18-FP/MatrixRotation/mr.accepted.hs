import Data.Array
import Data.List

toRing m n (r, c) = (ring, pos, len)
    where
        ring = minimum [r - 1, m - r, c - 1, n - c]
        len = 2 * ((m - (2 * ring) - 1) + (n - (2 * ring) - 1))
        pos =
            if c == ring + 1
                then r - ring - 1
                else
                    if r == m - ring
                        then (m - (2 * ring)) + (c - ring - 2)
                        else
                            if c == n - ring
                                then len - (n - (2 * ring)) - (r - ring - 2)
                                else len - (c - ring - 1) 

fromRing m n (ring, pos) = res
    where
        r0 = ring + 1
        c0 = ring + 1
        len = 2 * ((m - (2 * ring) - 1) + (n - (2 * ring) - 1))
        res =
            if pos < m - (2 * ring)
                then (r0 + pos, c0)
                else
                    if pos < m - (2 * ring) + n - (2 * ring) - 1
                        then (m - ring, c0 + (pos - (m - (2 * ring) - 1)))
                        else
                            if pos > len - (n - (2 * ring)) + 1
                                then (r0, c0 + (len - pos))
                                else (r0 + (len - pos - (n - (2 * ring) - 1)), n - ring)

rot r c m n rt = fromRing m n (ring, pos')
    where
        (ring, pos, len) = toRing m n (r, c)
        pos' = (pos + (len - (rt `mod` len))) `mod` len

readLine :: Int -> IO (Int, [(Int, Int)])
readLine n = do
    l <- getLine
    return (n, [1 ..] `zip` (read `map` (words l)))

main = do
    mnrstr <- getLine
    let (m : n : rt : _) = read `map` (words mnrstr)
    ls <- mapM readLine [1 .. m]
    let mx = array ((1, 1), (m, n)) [((r, c), x) | (r, xs) <- ls, (c, x) <- xs]
    mapM (\r -> putStrLn $ intercalate " " ((\c -> show $ mx ! (rot r c m n rt)) `map` [1 .. n])) [1 .. m]

