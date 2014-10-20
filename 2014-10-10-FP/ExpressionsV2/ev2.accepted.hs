import Data.Bits
import Data.Char
import Data.Int

mmm = 1000000007

eva :: (Int64 -> Int64) -> String -> (Int64, String)
eva f (' ' : cs) = eva f cs
eva f cs = evra f n cs'
    where
        (n, cs') = evm id cs

evra :: (Int64 -> Int64) -> Int64 -> String -> (Int64, String)
evra f n (')' : cs) = (f n, cs)
evra f n (' ' : cs) = evra f n cs
evra f n ('+' : cs) = eva (f . (plus n)) cs
evra f n ('-' : cs) = eva (f . (minus n)) cs
evra f n cs = (f n, cs)

evm :: (Int64 -> Int64) -> String -> (Int64, String)
evm f (' ' : cs) = evm f cs
evm f ('(' : cs) = evrm f n cs'
    where
        (n, cs') = eva id cs
evm f cs = evrm f n cs'
    where
        (n, cs') = evn 1 0 cs

evrm :: (Int64 -> Int64) -> Int64 -> String -> (Int64, String)
evrm f n (' ' : cs) = evrm f n cs
evrm f n ('*' : cs) = evm (f . (mult n)) cs
evrm f n ('/' : cs) = evm (f . (divn n)) cs
evrm f n cs = (f n, cs)

evn :: Int64 -> Int64 -> String -> (Int64, String)
evn f n (' ' : cs) = evn f n cs
evn f n ('+' : cs) = evn f n cs
evn f n ('-' : cs) = evn (-f) n cs
evn f n cs = evn' f n cs

evn' f n cs@(c : cs') =
    if c >= '0' && c <= '9'
        then evn' f (10 * n + (fromIntegral $ ord c) - 48) cs'
        else (f * n, cs)
evn' f n [] = (f * n, [])

mp acc _ 0 = acc
mp acc b e = mp (if odd e then ((acc * b) `mod` mmm) else acc) ((b * b) `mod` mmm) (shiftR e 1)

plus a b = (a + b) `mod` mmm
minus a b = (a - b) `mod` mmm
mult a b = (a * b) `mod` mmm
divn a b = (a * mp 1 b (mmm - 2)) `mod` mmm

main = do
    x <- getLine
    putStrLn $ show $ fst $ eva id x

