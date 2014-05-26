import Data.List
import qualified Data.Map as M
import Data.Maybe

comb :: [Integer] -> M.Map Integer Integer
comb xs = foldl g M.empty xs
    where
        g' Nothing x = M.insert x 1
        g' (Just n) x = M.insert x (succ n)
        g m x = g' (M.lookup x m) x m

f' k xs = nub $ filter (\x -> (fromJust $ M.lookup x m) >= k) xs
    where
        m = comb xs
        
f k xs =
        let res = f' k xs
            in
                if null res
                    then "-1"
                    else (unwords . (map show)) res
        
tst = do
        kstr <- getLine
        let (n : k : _) = map (read :: String -> Integer) (words kstr)
        astr <- getLine
        (putStrLn . (f k) . (map read) . words) astr

main = do
        tstr <- getLine
        mapM (const tst) [1 .. (read tstr)]

