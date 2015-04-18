import Data.List
import Data.Set as S

slv = reverse . fst . Data.List.foldl' f ("", S.empty)
    where
        f acc@(str, s) c =
            if c `S.member` s
                then acc
                else (c : str, c `S.insert` s)

main = do
    str <- getLine
    putStrLn $ slv str

