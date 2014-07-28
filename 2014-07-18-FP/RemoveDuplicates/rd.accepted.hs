import Data.Set

nb = (reverse . snd . (Prelude.foldl f (empty, [])))
    where
        f (s, acc) c =
            if c `member` s
                then (s, acc)
                else (c `insert` s, c : acc)

main = do
    str <- getLine
    putStrLn $ nb str

