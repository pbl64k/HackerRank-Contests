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

--

buildpaths cs x = if null xcs then ((1, [x]), []) else ((succ ln, x : xx), acclst)
    where
        xcs = cs ! x
        (xc : xcs') = xcs
        init = buildpaths cs xc
        f ((ln, xx), acclst) x' = if ln > ln' then ((ln, xx), (ln', xx') : (acclst ++ acclst')) else ((ln', xx'), (ln, xx) : (acclst ++ acclst'))
            where
                ((ln', xx'), acclst') = buildpaths cs x'
        ((ln, xx), acclst) = foldl f init xcs'

ext ps lvs paths extd lv xp =
    if extd <= dd
        then take extd $ drop (dd - extd) (elems p)
        else ext ps lvs paths (extd - dd) (pred xlv) xp' ++ (take extd $ elems p)
    where
        (d, p) = paths ! xp
        dd = (lv - fst (bounds p)) + 1
        x = head $ elems p
        xlv = lvs ! x
        xp' = ps ! x

extendpaths ps lvs paths r@(d, p) = [(x, res) | x <- elems p]
    where
        x = head $ elems p
        xlv = lvs ! x
        xp = ps ! x
        extd = min d (pred $ xlv)
        res = if xp == 0 then r else (extd + d, listArray (xlv - extd, xlv + d - 1) $ ext ps lvs paths extd (pred xlv) xp ++ elems p)

fGe lad tlv blv tgtGe =
    if (succ tlv) == blv
        then blv
        else
            if lad ! midp == tgtGe
                then midp
                else
                    if lad ! midp < tgtGe
                        then fGe lad midp blv tgtGe
                        else fGe lad tlv midp tgtGe
    where
        midp = (tlv + blv) `div` 2

findGe lad ps botlv botx tgtGe =
    if topx == tgtGe
        then toplv
        else
            if botx == tgtGe
                then botlv
                else
                    if topx < tgtGe
                        then fGe curlad toplv botlv tgtGe
                        else
                            if ps ! topx == 0 || (lad ! (ps ! topx)) ! (pred toplv) < tgtGe
                                then toplv
                                else findGe lad ps (pred toplv) (ps ! topx) tgtGe
    where
        curlad = (lad ! botx)
        toplv = fst $ bounds curlad
        topx = curlad ! toplv

fLe lad tlv blv tgtLe =
    --(unsafePerformIO $ putStrLn $ "aa: " ++ show (tlv, midp, blv)) `seq`
    if (succ tlv) == blv
        then tlv
        else
            if lad ! midp == tgtLe
                then midp
                else
                    if lad ! midp < tgtLe
                        then fLe lad midp blv tgtLe
                        else fLe lad tlv midp tgtLe
    where
        midp = (tlv + blv) `div` 2

findLe lad ps botlv botx tgtLe =
    --(unsafePerformIO $ putStrLn $ show (topx, tgtLe, toplv)) `seq`
    if topx == tgtLe
        then toplv
        else
            if botx <= tgtLe
                then botlv
                else
                    if topx < tgtLe
                        then fLe curlad toplv botlv tgtLe
                        else findLe lad ps (pred toplv) (ps ! topx) tgtLe
    where
        curlad = (lad ! botx)
        toplv = fst $ bounds curlad
        topx = curlad ! toplv

cmp lad lvs ps l r l' r' =
    --(unsafePerformIO $ putStrLn $ show (l, r, l', r', llv, rlv)) `seq`
    res
    where
        res = if r < l' || r' < l then 0 else (rlv - llv + 1)
        --llv = if l <= l' && l' <= r then lvs ! l' else snd $ fromJust $ l `M.lookupGE` t
        llv = if l <= l' && l' <= r then lvs ! l' else findGe lad ps (lvs ! r') r' l
        --rlv = if l <= r' && r' <= r then lvs ! r' else snd $ fromJust $ r `M.lookupLE` t
        rlv = if l <= r' && r' <= r then lvs ! r' else findLe lad ps (lvs ! r') r' r

-- Misc

(a1, b1) `intersects` (a2, b2) =
    (a2 <= a1 && a1 <= b2) ||
    (a2 <= b1 && b1 <= b2) ||
    (a1 <= a2 && a2 <= b1)

cmidp a b = (a + b) `div` 2 + 1

buildlvs cs lv x = (x, lv) : ((cs ! x) >>= (buildlvs cs (succ lv)))

swap (x, y) = (y, x)

readLst = return . (read `map`) . words

convp lvs (d, xs@(x : xs')) = (d, listArray (lvs ! x, lvs ! x + (d - 1)) xs)

--

tst _ 0 = return ()
tst pc@((arrlad, arrlvs, arrps), lcas) m = do
    (x : y : l : r : _) <- getLine >>= readLst
    let xyLca = lca lcas x y
    let f = cmp arrlad arrlvs arrps
    --putStrLn $ "LCA: " ++ show xyLca
    --putStrLn $ "x: " ++ show (f l r xyLca x)
    --putStrLn $ "y: " ++ show (f l r xyLca y)
    --putStrLn $ "xylca: " ++ show (f l r xyLca xyLca)
    putStrLn $ show (f l r xyLca x + f l r xyLca y - f l r xyLca xyLca)
    tst pc (pred m)

main = do
    (n : m : _) <- getLine >>= readLst
    ps <- getLine >>= readLst
    let arrps = listArray (1, n) (0 : ps)
    let lstcs = [1 .. n] `zip` [[] | _ <- [1 ..]] ++ ps `zip` ((: []) `map` [2 ..])
    let mcs = M.fromListWith (flip (++)) lstcs
    let arrcs = array (1, n) (M.toList mcs)
    let lstlvs = buildlvs arrcs 1 1
    let arrlvs = array (1, n) lstlvs
    -- LAP
    let (pp, paths) = buildpaths arrcs 1
    let rp = (convp arrlvs) `map` (pp : paths)
    let arrpaths = array (1, n) [(x, p) | p@(_, path) <- rp, x <- elems path]
    let ladders = rp >>= (extendpaths arrps arrlvs arrpaths)
    let arrlad = array (1, n) ((\(a, (_, b)) -> (a, b)) `map` ladders)
    --putStrLn $ show arrlad
    -- LCA
    let (lstseq, lstmap, finalix) = buildrmq ([], [], 1) arrcs arrlvs 1
    let arrseq = array (1, finalix - 1) lstseq
    let leftmap = M.toList $ M.fromListWith min lstmap
    let rightmap = M.toList $ M.fromListWith max lstmap
    let arrlm = array (1, n) leftmap
    let arrrm = array (1, n) rightmap
    let (minn, lcast) = buildst arrseq 1 (pred finalix)
    tst ((arrlad, arrlvs, arrps), (lcast, arrlm, arrrm)) m
    --return ()

