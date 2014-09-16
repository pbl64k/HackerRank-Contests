import Data.List

quad True True = 1
quad True False = 2
quad False True = 3
quad False False = 4

adj m f q [r, c] = [r', c']
    where
        off = 2 ^ (m - 1)
        r' = if q `elem` [3, 4] then r + (f * off) else r
        c' = if q `elem` [2, 4] then c + (f * off) else c

solve 0 _ _ _ = return ()
solve m [r0, c0] r c = do
    let rc = 2 ^ (m - 1)
    let q = quad (r <= rc) (c <= rc)
    let sqs = [(1, [rc, rc]), (2, [rc, rc + 1]), (3, [rc + 1, rc]), (4, [rc + 1, rc + 1])]
    let sqs' = " " `intercalate` (show `map` (concat (((\(r' : c' : _) -> [r' + r0, c' + c0]) . snd) `map` (((/= q) . fst) `filter` sqs))))
    let sqs'' = (\(sq, coords) -> if q == sq then (sq, adj m (-1) sq [r, c]) else (sq, adj m (-1) sq coords)) `map` sqs
    putStrLn sqs'
    mapM_ (\(sq, [r1, c1]) -> solve (pred m) (adj m 1 sq [r0, c0]) r1 c1) sqs''

main = do
    m <- getLine >>= (return . read)
    rcstr <- getLine
    let (r : c : _) = read `map` (words rcstr)
    solve m [0, 0] r c

