import Data.List

data T = Leaf | Node T Integer Integer T deriving Show

mktree 0 xs = (Leaf, xs)
mktree 1 ((n, s):xs) = (Node Leaf n s Leaf, xs)
mktree n xs =
        let midp = (succ n) `div` 2
            (l, ((n', s'):xs')) = mktree (pred midp) xs
            (r, xs'') = mktree (n - midp) xs'
            in (Node l n' s' r, xs'')

tfind Leaf _ = -1
tfind (Node l n s r) x =
        if x == s
            then n
            else
                if x < s
                    then
                        let l' = tfind l x
                            in
                                if l' == -1
                                    then n
                                    else l'
                    else tfind r x

-- Incredibly, removing the wrapping lambda damages the performance badly
-- enough to time out on most of the tests. After all these years, Haskell
-- is still beyond me.
tst t = getLine >>= (\sstr -> (putStrLn . show . (tfind t) . read) sstr)

main = do
        getLine
        astr <- getLine
        let as = sortBy (flip compare) $ map read (words astr)
        let a's = zip [1 ..] ((reverse . snd) $ foldl (\(s, l) x -> (s + x, (s + x):l)) (0, []) as)
        let (t, _) = mktree (length a's) a's
        tstr <- getLine
        mapM_ (const (tst t)) [1 .. (read tstr)]

