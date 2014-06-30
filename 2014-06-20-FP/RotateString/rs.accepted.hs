import Data.Foldable
import Data.List
import Data.Sequence

rotate 0 _ = []
rotate n str = (Data.Foldable.foldr (:) [] str') : rotate (pred n) str'
    where
        (x :< xs) = viewl str
        str' = xs |> x

tst = do
    str <- getLine
    putStrLn (intercalate " " $ rotate (Prelude.length str) (fromList str))

main = do
    tstr <- getLine
    Prelude.mapM_ (const tst) [1 .. (read tstr)]

