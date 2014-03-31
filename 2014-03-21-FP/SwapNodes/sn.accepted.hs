import Data.Array
import Data.List

data T = L | N T Int T deriving Show

traverse acc L = acc
traverse acc (N l x r) = traverse (x : (traverse acc r)) l

swap _ _ L = L
swap k lvl (N l x r) =
    if lvl `mod` k == 0
        then N (swap k (succ lvl) r) x (swap k (succ lvl) l)
        else N (swap k (succ lvl) l) x (swap k (succ lvl) r)

mktree :: [(Int, Int)] -> T
mktree flat = tlist ! 1
    where
        tlist = array (1, length flat) (map (\(n, (a, b)) -> (n, N (f a) n (f b))) (zip [1 ..] flat))
        f n = if n == (-1) then L else tlist ! n

children :: IO (Int, Int)
children = do
    cstr <- getLine
    let (astr, bstr) = span (/= ' ') cstr
    let a = read astr
    let b = read $ tail bstr
    return (a, b)

tst 0 tree = return ()
tst n tree = do
    kstr <- getLine
    let newtree = swap (read kstr) 1 tree
    putStrLn (intercalate " " (map show (traverse [] newtree)))
    tst (n - 1) newtree

main = do
    nstr <- getLine
    flat <- mapM (const children) [1 .. (read nstr)]
    let tree = mktree flat
    tstr <- getLine
    tst (read tstr) tree

