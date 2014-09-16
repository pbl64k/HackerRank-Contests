import Data.Array
import qualified Data.Map as M
import Data.Maybe

import System.IO.Unsafe

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

-- PTree

data PTree = PtLeaf | PtNode (Int, Int) PTree PTree deriving (Eq, Ord, Show)

buildpt 0 acc = (acc, PtLeaf)
buildpt n acc = (acc'', PtNode x lt rt)
    where
        midp = cmidp 0 n
        (x : acc', lt) = buildpt (pred midp) acc
        (acc'', rt) = buildpt (n - midp) acc'

buildns cs lvs acc (n, accn) x = ((myn, myt), (x, myt) : acc')
    where
        accn' = (x, lvs ! x) : accn
        mcs = cs ! x
        f ((nn, t), acc) c = (if nn' < nn then (nn', t') else (nn, t), acc')
            where
                ((nn', t'), acc') = buildns cs lvs acc (succ n, accn') c
        (acc', myn, myt) =
            if null mcs
                then
                    let (_, t) = buildpt (succ n) accn'
                        in (acc, n, t)
                else
                    let (mmc : mmcs) = mcs
                        init = buildns cs lvs acc (succ n, accn') mmc
                        ((n', t'), acc') = foldl f init mmcs
                        in (acc', n', t')

jnl (Just (x', lv')) (x, lv) = if x' < x then Just (x', lv') else Just (x, lv)

ptfl PtLeaf acc _ = acc
ptfl (PtNode n@(x, lv) lt rt) Nothing tgt =
    if x == tgt
        then Just n
        else
            if x < tgt
                then ptfl lt Nothing tgt
                else ptfl rt (Just n) tgt
ptfl (PtNode n@(x, lv) lt rt) acc tgt =
    if x == tgt
        then Just n
        else
            if x < tgt
                then acc
                else ptfl rt (jnl acc n) tgt

jnr (Just (x', lv')) (x, lv) = if x' > x then Just (x', lv') else Just (x, lv)

ptfr PtLeaf acc _ = acc
ptfr (PtNode n@(x, lv) lt rt) Nothing tgt =
    if x == tgt
        then Just n
        else
            if x > tgt
                then ptfr rt Nothing tgt
                else ptfr lt (Just n) tgt
ptfr (PtNode n@(x, lv) lt rt) acc tgt =
    if x == tgt
        then Just n
        else
            if x > tgt
                then acc
                else ptfr lt (jnr acc n) tgt

cmp lvs t l r l' r' = if r < l' || r' < l then 0 else (rlv - llv + 1)
    where
        llv = if l <= l' && l' <= r then lvs ! l' else snd $ fromJust $ ptfl t Nothing l
        rlv = if l <= r' && r' <= r then lvs ! r' else snd $ fromJust $ ptfr t Nothing r

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
tst pc@(arrlvs, arrns, lcas) m = do
    (x : y : l : r : _) <- getLine >>= readLst
    let xyLca = lca lcas x y
    --putStrLn $ "LCA: " ++ show xyLca
    putStrLn $ show (cmp arrlvs (arrns ! x) l r xyLca x + cmp arrlvs (arrns ! y) l r xyLca y - cmp arrlvs (arrns ! xyLca) l r xyLca xyLca)
    tst pc (pred m)

main = do
    (n : m : _) <- getLine >>= readLst
    ps <- getLine >>= readLst
    let lstcs = [1 .. n] `zip` [[] | _ <- [1 ..]] ++ ps `zip` ((: []) `map` [2 ..])
    let mcs = M.fromListWith (flip (++)) lstcs
    let arrcs = array (1, n) (M.toList mcs)
    let lstlvs = buildlvs arrcs 1 1
    let arrlvs = array (1, n) lstlvs
    let (_, lstns) = buildns arrcs arrlvs [] (0, []) 1
    let arrns = array (1, n) lstns
    let (lstseq, lstmap, finalix) = buildrmq ([], [], 1) arrcs arrlvs 1
    let arrseq = array (1, finalix - 1) lstseq
    let leftmap = M.toList $ M.fromListWith min lstmap
    let rightmap = M.toList $ M.fromListWith max lstmap
    let arrlm = array (1, n) leftmap
    let arrrm = array (1, n) rightmap
    let (minn, lcast) = buildst arrseq 1 (pred finalix)
    tst (arrlvs, arrns, (lcast, arrlm, arrrm)) m
    --return ()

