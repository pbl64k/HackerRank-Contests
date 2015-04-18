import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Array
import Data.List

mmm = 1000000007

type Pfs = [(Int, Int)]

pf n = pf' 2 n
    where
        nd n d =
            if n `mod` d == 0
                then 1 + nd (n `div` d) d
                else 0
        pf' _ 1 = []
        pf' d n =
            if x > 0
                then (d, x) : pf' (succ d) (n `div` d ^ x)
                else pf' (succ d) n
            where
                x = nd n d

pfs = listArray (1, 100) (pf `map` [1 .. 100])

data SegT = STLeaf Int Pfs | STNode Int Int Int Pfs SegT SegT deriving Show

tpfs (STLeaf _ pfs) = pfs
tpfs (STNode _ _ _ pfs _ _) = pfs

lcm' [] ys = ys
lcm' xs [] = xs
lcm' xs@((x, n) : xs') ys@((y, m) : ys') =
    if x < y
        then (x, n) : lcm' xs' ys
        else
            if y < x
                then (y, m) : lcm' xs ys'
                else (x, max n m) : lcm' xs' ys'

tlcm lt rt = lcm' (tpfs lt) (tpfs rt)

[] `mul` ys = ys
xs `mul` [] = xs
xs@((x, n) : xs') `mul` ys@((y, m) : ys') =
    if x < y
        then (x, n) : mul xs' ys
        else
            if y < x
                then (y, m) : mul xs ys'
                else (x, n + m) : mul xs' ys'

buildt :: Int -> Int -> [Pfs] -> (SegT, [Pfs])
buildt l r pfs@(pf : pfs') =
    if l == r
        then (STLeaf l pf, pfs')
        else (STNode l midp r (tlcm lt rt) lt rt, pfs''')
    where
        midp = l + (r - l) `div` 2
        (lt, pfs'') = buildt l midp pfs
        (rt, pfs''') = buildt (succ midp) r pfs''

updmult :: Int -> Pfs -> SegT -> SegT
updmult _ pfsm (STLeaf ix pfs) = STLeaf ix (pfs `mul` pfsm)
updmult ix pfsm (STNode l midp r pfs lt rt) =
    if ix <= midp
        then
            let lt' = updmult ix pfsm lt
                in STNode l midp r (tlcm lt' rt) lt' rt
        else
            let rt' = updmult ix pfsm rt
                in STNode l midp r (tlcm lt rt') lt rt'

queryt :: Int -> Int -> SegT -> Pfs
queryt ql qr (STLeaf ix pf) =
    if ql <= ix && ix <= qr
        then pf
        else []
queryt ql qr (STNode l _ r pfs lt rt) =
    if ql <= l && r <= qr
        then pfs
        else
            if qr < l || r < ql
                then []
                else lcm' (queryt ql qr lt) (queryt ql qr rt)

x `expmod` 1 = x
x `expmod` p =
    if p `mod` 2 == 0
        then ((x * x) `mod` mmm) `expmod` (p `div` 2) 
        else (x * (x `expmod` (pred p))) `mod` mmm
                    
prc ('Q' : ' ' : cmd) = do
    let (l : r : _) = map read $ words cmd
    t <- get
    let pfs = queryt l r t
    liftIO $ putStrLn $ show $ foldl' (\acc (x, n) -> (acc * (x `expmod` n)) `mod` mmm) 1 pfs
prc ('U' : ' ' : cmd) = do
    let (ix : n : _) = map read $ words cmd
    t <- get
    let t' = updmult ix (pfs ! n) t
    put t'

test :: Int -> StateT SegT IO ()
test 0 = return ()
test k = do
    cmd <- liftIO $ getLine
    prc cmd
    test (pred k)

main = do
    nstr <- getLine
    let n = read nstr
    xstr <- getLine
    let xs = map read $ words xstr
    kstr <- getLine
    let k = read kstr
    let xpfs = map (pfs !) xs
    let t = fst $ buildt 0 (pred n) xpfs
    evalStateT (test k) t

