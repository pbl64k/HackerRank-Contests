import Data.Array
import Data.List

--import System.IO.Unsafe

-- Unholy union of segment trees and generalized Kadane's algo

data Sub = Sub Int Int Int deriving (Show, Eq)

ssc (Sub s _ _) = s

sl (Sub _ l _) = l

sr (Sub _ _ r) = r

(s, a@(Sub as al ar)) >=< b@(Sub bs bl br) =
    if as == 0 && al >= bl
        then b
        else
            if bs == 0 && br <= ar
                then a
                else
                    if succ ar == bl
                        then Sub (as + bs) al br
                        else error $ "Cannot glue mismatched subs " ++ s ++ " " ++ show a ++ " and " ++ show b

instance Ord Sub where
    -- use compare?
    a@(Sub as al ar) <= b@(Sub bs bl br) = a == b || as > bs ||
        (as == bs && (al < bl || (al == bl && ar < br)))

data STree = ST {
    tot :: Sub,
    best :: Sub,
    left :: Sub,
    right :: Sub,
    sts :: [STree] }
    deriving Show

buildSt l r xs =
    --(unsafePerformIO $ putStrLn $ show (l, r)) `seq`
    if l == r
        then
            let t = Sub (xs ! l) l r
                in
                    --(unsafePerformIO $ putStrLn $ show (min t (Sub 0 l r))) `seq`
                    ST { tot = t, best = min t (Sub 0 l r),
                    left = min t (Sub 0 (pred l) (pred l)), right = min t (Sub 0 (succ r) (succ r)),
                    sts = [] }
        else
            let midp = (l + r) `div` 2
                lt = buildSt l midp xs
                rt = buildSt (succ midp) r xs
                in
                    --(unsafePerformIO $ putStrLn $ show (lt, rt)) `seq`
                    ST { tot = ("a", tot lt) >=< tot rt, best = minimum [best lt, best rt, ("b", right lt) >=< left rt],
                    left = min (left lt) (("c", tot lt) >=< left rt), right = min (right rt) (("d", right lt) >=< tot rt),
                    sts = [lt, rt] }

bst :: [STree] -> (Sub, Sub)
bst sts = foldl' f (def, def) sts
    where
        ll = if null sts then (-1) else sl $ tot $ head sts
        def = Sub 0 ll ll
        f (bs, ls) st = (minimum [bs, best st, ("e", ls) >=< left st], min (right st) (("f", ls) >=< tot st))

splitlr :: Int -> Int -> [STree] -> ([STree], [STree])
splitlr l r sts = (ls, splitr r sts')
    where
        (sts', ls) = splitl l sts

splitl :: Int -> [STree] -> ([STree], [STree])
splitl _ [] = ([], [])
splitl l psts@(pst : psts') =
    if l <= sl (tot pst)
        then (psts, [])
        else
            if sr (tot pst) < l
                then
                    let (rem, ls) = splitl l psts'
                        in (rem, pst : ls)
                else
                    let (rem, ls) = splitl l (sts pst)
                        in (rem ++ psts', ls)

splitr :: Int -> [STree] -> [STree]
splitr _ [] = []
splitr r psts@(pst : psts') =
    if sr (tot pst) <= r
        then splitr r psts'
        else
            if r < sl (tot pst)
                then psts
                else splitr r (sts pst) ++ psts'

merger :: [Sub] -> [Sub] -> [Sub]
merger [] ys = ys
merger xs [] = xs
merger xs@(x : xs') ys@(y : ys') =
    if x <= y
        then x : merger xs' ys
        else y : merger xs ys'

slv :: [STree] -> [Sub]
slv sts = res : merger (slv lts) (slv rts)
    where
        res@(Sub sc l r) = fst $ bst sts
        (lts, rts) = splitlr l r sts

main = do
    fstr <- getLine
    let (n : k : []) = read `map` (words fstr)
    astr <- getLine
    let xs = (read :: String -> Int) `map` (words astr)
    let xarr = listArray (0, n - 1) xs
    let st = buildSt 0 (pred n) xarr
    putStrLn $ "\n" `intercalate` ((show . ssc) `map` (takeWhile ((> 0) . ssc) $ take k $ slv [st]))

