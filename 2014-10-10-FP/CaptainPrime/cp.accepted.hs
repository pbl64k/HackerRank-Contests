import qualified Data.IntSet as S
import Data.List

slv ps p =
    if not c || '0' `elem` p
        then "DEAD"
        else
            if l && r
                then "CENTRAL"
                else
                    if l
                        then "RIGHT" -- oops
                        else
                            if r
                                then "LEFT"
                                else "DEAD"
    where
        c = (read p) `S.member` ps
        l = all (\x -> (read x) `S.member` ps) (tail $ inits p)
        r = all (\x -> (read x) `S.member` ps) (init $ tails p)

tst ps = do
    p <- getLine
    putStrLn $ slv ps p

main = do
    let ps = S.fromList $ takeWhile (<= 1000000) fastprimes
    tstr <- getLine
    mapM_ (const $ tst ps) [1 .. read tstr]

-- Ye olde Eulerian mojo follows

-- Pairing heaps

data Ph a = EmptyPh | Ph a [Ph a] deriving Show

singPh x = Ph x []

minPh (Ph x _) = x

mergePh x EmptyPh = x
mergePh EmptyPh x = x
mergePh x@(Ph a as) y@(Ph b bs) =
    if a < b
        then Ph a (y : as)
        else Ph b (x : bs)

insertPh x e = x `mergePh` (singPh e)

delminPh (Ph _ xs) = mergePPh xs

mergePPh [] = EmptyPh
mergePPh [x] = x
mergePPh (x : y : rest) = (x `mergePh` y) `mergePh` (mergePPh rest)

-- O'Neill, The Genuine Sieve of Eratosthenes
fastprimes :: Integral a => [a]
fastprimes = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)

wheel2357 :: Integral a => [a]
wheel2357 = 2 : 4 : 2 : 4 : 6 : 2 : 6 : 4 : 2 : 4 : 6 : 6 : 2 : 6 : 4 : 2 : 6 : 4 : 6 : 8 : 4 : 2 : 4 : 2 : 4 : 8 : 6 : 4 : 6 : 2 : 4 : 6 : 2 : 6 : 6 : 4 : 2 : 4 : 6 : 2 : 6 : 4 : 2 : 4 : 2 : 10 : 2 : 10 : wheel2357

spin :: Integral a => [a] -> a -> [a]
spin (x : xs) n = n : spin xs (n + x)

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (x : xs) = x : sieve' xs (insertprime x xs EmptyPh)
    where
        insertprime p xs table = insertPh table ((p * p), ((* p) `map` xs))
        sieve' [] table = []
        sieve' (x : xs) table
            | nextComposite <= x = sieve' xs (adjust table)
            | otherwise = x : sieve' xs (insertprime x xs table)
                where
                    nextComposite = fst $ minPh table
                    adjust table
                        | n <= x = adjust (insertPh (delminPh table) (n', ns))
                        | otherwise = table
                            where
                                (n, n' : ns) = minPh table

