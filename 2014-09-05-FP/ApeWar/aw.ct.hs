import Data.Array
import qualified Data.Map as M

--import System.IO.Unsafe

-- SegTree

data SegTree a = StLeaf Int a | StNode Int Int a (SegTree a) (SegTree a) deriving (Eq, Ord, Show)

buildrmq acc cs lvs x = acc'
    where
        updacc (lstseq, lstmap, ix) = (lstseq', lstmap', succ ix)
            where
                lstseq' = (ix, ((lvs ! x), x)) : lstseq
                lstmap' = (x, ix) : lstmap
        f acc c = updacc acc'
            where
                acc' = buildrmq acc cs lvs c
        acc' = foldl f (updacc acc) (cs ! x)

buildst arrseq a b =
    if a == b
        then
            let minx = arrseq ! a
                in (minx, StLeaf a minx)
        else
            let midp = cmidp a b
                (minl, lt) = buildst arrseq a (midp - 1)
                (minr, rt) = buildst arrseq midp b
                minx = min minl minr
                in (minx, StNode a b minx lt rt)

lcast (StLeaf _ x) _ _ = x
lcast (StNode l r x lt rt) a b =
    if a <= l && r <= b
        then x
        else minimum $ (\(_, t) -> lcast t a b) `map` rs
    where
        midp = cmidp l r
        rs = (((a, b) `intersects`) . fst) `filter` [((l, midp - 1), lt), ((midp, r), rt)]

lca (st, lm, rm) a b =
    --(unsafePerformIO $ putStrLn $ show (al, ar, bl, br)) `seq`
    if al <= bl && br <= ar
        then a
        else
            if bl <= al && ar <= br
                then b
                else
                    if al < bl
                        then snd $ lcast st ar bl
                        else snd $ lcast st br al
    where
        al = lm ! a
        ar = rm ! a
        bl = lm ! b
        br = rm ! b

-- CTree

data CTree = CtNone Int Int | CtAll Int Int | CtNode Int Int Int CTree CTree deriving (Eq, Ord, Show)

ctmidp (CtNone a b) = cmidp a b
ctmidp (CtAll a b) = cmidp a b
ctmidp (CtNode a b _ _ _) = cmidp a b

t@(CtNone a b) `ctins` x =
    if a == b
        then CtAll a b
        else
            if x < midp
                then CtNode a b 1 ((CtNone a (midp - 1)) `ctins` x) (CtNone midp b)
                else CtNode a b 1 (CtNone a (midp - 1)) ((CtNone midp b) `ctins` x)
    where
        midp = ctmidp t
t@(CtNode a b n lt rt) `ctins` x =
    if b - a == n
        then CtAll a b
        else
            if x < midp
                then CtNode a b (n + 1) (lt `ctins` x) rt
                else CtNode a b (n + 1) lt (rt `ctins` x)
    where
        midp = ctmidp t

cntrng (CtNone _ _) _ _ = 0
cntrng (CtAll a b) l r =
    if l <= a && b <= r
        then b - a + 1
        else
            if a <= l && r <= b
                then r - l + 1
                else
                    if a <= l && l <= b
                        then b - l + 1
                        else
                            if a <= r && r <= b
                                then r - a + 1
                                else 0
cntrng (CtNode a b n lt rt) l r =
    if l <= a && b <= r
        then n
        else
            if (a <= l && l <= b) || (a <= r && r <= b)
                then cntrng lt l r + cntrng rt l r 
                else 0

buildns t cs x = (x, t') : cts
    where
        t' = t `ctins` x
        cts = (cs ! x) >>= (buildns t' cs)

-- Misc

(a1, b1) `intersects` (a2, b2) =
    (a2 <= a1 && a1 <= b2) ||
    (a2 <= b1 && b1 <= b2) ||
    (a1 <= a2 && a2 <= b1)

cmidp a b = (a + b) `div` 2 + 1

buildlvs cs lv x = (x, lv) : ((cs ! x) >>= (buildlvs cs (succ lv)))

swap (x, y) = (y, x)

readLst = return . (read `map`) . words

--

tst _ 0 = return ()
tst pc@(arrns, lcas) m = do
    (x : y : l : r : _) <- getLine >>= readLst
    let xyLca = lca lcas x y
    --putStrLn $ "LCA: " ++ show xyLca
    putStrLn $ show (cntrng (arrns ! x) l r + cntrng (arrns ! y) l r - (2 * cntrng (arrns ! xyLca) l r) + if l <= xyLca && xyLca <= r then 1 else 0)
    tst pc (pred m)

main = do
    (n : m : _) <- getLine >>= readLst
    ps <- getLine >>= readLst
    let lstcs = [1 .. n] `zip` [[] | _ <- [1 ..]] ++ ps `zip` ((: []) `map` [2 ..])
    let mcs = M.fromListWith (flip (++)) lstcs
    let arrcs = array (1, n) (M.toList mcs)
    let lstlvs = buildlvs arrcs 1 1
    let arrlvs = array (1, n) lstlvs
    let lstns = buildns (CtNone 1 n) arrcs 1
    let arrns = array (1, n) lstns
    --putStrLn $ show arrns
    let (lstseq, lstmap, finalix) = buildrmq ([], [], 1) arrcs arrlvs 1
    let arrseq = array (1, finalix - 1) lstseq
    let leftmap = M.toList $ M.fromListWith min lstmap
    let rightmap = M.toList $ M.fromListWith max lstmap
    let arrlm = array (1, n) leftmap
    let arrrm = array (1, n) rightmap
    let (minn, lcast) = buildst arrseq 1 (pred finalix)
    tst (arrns, (lcast, arrlm, arrrm)) m
    --return ()

