{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Array
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import qualified Data.Sequence as Sq

data BinTree a = BtEmpty | BtLeaf a | BtNode a (BinTree a) (BinTree a) deriving (Show, Eq, Ord)
data RangeTree a b = RtEmpty | RtLeaf a (BinTree b) | RtNode a a (BinTree b) (RangeTree a b) (RangeTree a b) deriving (Show, Eq, Ord)

rtmake :: (Ord a, Ord b) => [(a, b)] -> RangeTree a b
rtmake xys = res
    where
        (res, _, _) = rtcons (length xys) (sort xys)

rtcons 0 [] = (RtEmpty, (0, []), Nothing)
rtcons 1 [(x, y)] = (RtLeaf x (BtLeaf y), (1, [y]), Just (x, x))
rtcons n xs = (RtNode l r bt lrt rrt, btp, Just (l, r))
    where
        lsz = (n `div` 2)
        rsz = n - lsz
        (xl, xr) = splitAt lsz xs
        (lrt, lbt, lbnd) = rtcons lsz xl
        (rrt, rbt, rbnd) = rtcons rsz xr
        Just (l, r) = mrgbnd lbnd rbnd
            where
                mrgbnd Nothing r = r
                mrgbnd l Nothing = l
                mrgbnd (Just (l, _)) (Just (_, r)) = Just (l, r)
        btp = merge lbt rbt
        (bt, []) = btcons btp

btcons (0, xs) = (BtEmpty, xs)
btcons (1, (x : xs)) = (BtLeaf x, xs)
btcons (n, xs) = (BtNode x l r, xs'')
    where
        m = n `div` 2
        (l, (x : xs')) = btcons (m, xs)
        (r, xs'') = btcons (n - m - 1, xs')

merge (nl, xl) (nr, xr) = (nl + nr, merge' xl xr)
    where
        merge' xs [] = xs
        merge' [] ys = ys
        merge' xs@(x : xs') ys@(y : ys')
            | y < x     = y : merge' xs ys'
            | otherwise = x : merge' xs' ys

rtsplit RtEmpty _ _ = (Sq.empty, Sq.empty)
rtsplit (RtLeaf n bt) ql qr
    | n < ql     = (Sq.singleton bt, Sq.empty)
    | qr < n     = (Sq.empty, Sq.singleton bt)
    | otherwise  = (Sq.empty, Sq.empty)
rtsplit t@(RtNode l r bt lt rt) ql qr
    | r < ql    = (Sq.singleton bt, Sq.empty)
    | qr < l    = (Sq.empty, Sq.singleton bt)
    -- Seq?
    | otherwise = (ltl Sq.>< rtl, ltr Sq.>< rtr)
        where
            (ltl, ltr) = rtsplit lt ql qr
            (rtl, rtr) = rtsplit rt ql qr
    
maybeidop f (Just x) (Just y) = Just $ f x y
maybeidop _ jx@(Just _) _ = jx
maybeidop _ _ jy@(Just _) = jy
maybeidop _ _ _ = Nothing

maybemax = maybeidop max

maybemin = maybeidop min

btlbound BtEmpty _ = Nothing
btlbound (BtLeaf x') x
    | x' < x    = Just x'
    | otherwise = Nothing
btlbound (BtNode x' l r) x
    | x' < x = maybemax (Just x') (btlbound r x)
    | otherwise = btlbound l x

btubound BtEmpty _ = Nothing
btubound (BtLeaf x') x
    | x' > x    = Just x'
    | otherwise = Nothing
btubound (BtNode x' l r) x
    | x' > x = maybemin (Just x') (btubound l x)
    | otherwise = btubound r x

btslbound x = F.foldl' (\acc t -> maybemax acc (btlbound t x)) Nothing

btsubound x = F.foldl' (\acc t -> maybemin acc (btubound t x)) Nothing

query rngt t low upr = (l, r)
    where
        (lows, uprs) = rtsplit rngt low upr
        l = btslbound t (lows Sq.>< uprs)
        r = btsubound t (lows Sq.>< uprs)

--tstt = rtmake $ zip [3, 5, 2, 6, 1] [0 ..]

test _ _ _ 0 = return ()
test n aarr art q = do
    qstr <- getLine
    let (d : m : _) = map read $ words qstr
    let k = aarr ! d
    let (rl, rr) = query art d k (k + m)
    let xl = fromMaybe (-1) rl
    let xr = fromMaybe n rr
    putStrLn $ show ((d - xl - 1) + (xr - d - 1) + 1)
    test n aarr art (pred q)

main = do
    nstr <- getLine
    let n = read nstr
    astr <- getLine
    let as = map read $ words astr
    let art = rtmake (zip as [0 ..])
    let aarr = listArray (0, pred n) as
    qstr <- getLine
    let q = read qstr
    test n aarr art q

