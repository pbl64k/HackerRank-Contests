
{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Maybe

data Mintree = Leaf !Integer !Integer
             | Node !Integer !Integer !Integer !Mintree !Mintree deriving Show

tmin (Leaf m _) = m
tmin (Node m _ _ _ _) = m

buildtree xs = snd (buildtree' 0 (fromIntegral $ (length xs) - 1) xs)

buildtree' a b xs@(x:xs') =
    if a == b
       then (xs', Leaf x a)
       else let c = a + (b - a) `div` 2
                (xs', left) = buildtree' a c xs
                (xs'', right) = buildtree' (c + 1) b xs'
             in (xs'', Node (min (tmin left) (tmin right)) a b left right)

mm a Nothing = a
mm Nothing b = b
mm (Just a) (Just b) = Just $ min a b

findmin (Leaf m a') a b = if a' >= a && a' <= b then Just m else Nothing
findmin (Node m a' b' l r) a b =
    if a <= a' && b' <= b
       then Just m
       else if b < a' || b' < a
               then Nothing
               else mm (findmin l a b) (findmin r a b)

tst t = do
    l <- getLine
    let (a:b:[]) = str2numlist l
    putStrLn (show (fromJust $ findmin t a b))

main = do
    l1 <- getLine
    let (n:m:[]) = str2numlist l1
    l2 <- getLine
    let !t = buildtree $ str2numlist l2
    mapM_ (const (tst t)) [1 .. m]

str2numlist :: String -> [Integer]
str2numlist = (map read) . explode

explode = unfoldr f
    where f !str = let (!chunk, !rest) = span (/= ' ') str
                   in if null chunk
                         then Nothing
                         else if null rest
                                 then Just (chunk, rest)
                                 else Just (chunk, tail rest)

