import Data.List
--import qualified Data.Map as M
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ratio

inj [] (a, b, ix) = [(0, (a, b, ix))]
inj acc@((l, (a', b', ix')) : acc') (a, b, ix) =
    if a == a'
        then acc
        else
            let x = (b' - b) % (a - a')
                xn = numerator x
                xd = denominator x
                in
                    if x > l % 1
                        then (ceiling x, (a, b, ix)) : acc
                        else
                            if x == l % 1
                                then
                                    if ix' > ix
                                        then acc
                                        else (ceiling x, (a, b, ix)) : acc'
                                else
                                    inj acc' (a, b, ix)

getix (_, (_, _, ix)) = ix

test _ 0 = return ()
test mp q = do
    qstr <- getLine
    let qix = read qstr
    putStrLn $ show $ getix $ fromJust $ M.lookupLE qix mp
    test mp (pred q)

main = do
    pstr <- getLine
    let (n : q : _) = map (read :: String -> Integer) $ words pstr
    bstr <- getLine
    let bs = map (read :: String -> Integer) $ words bstr
    astr <- getLine
    let as = map (read :: String -> Integer) $ words astr
    let eqs = sortBy f $ zip3 as bs [1 ..]
            where
                f (a, b, x) (a', b', x') =
                    if ar /= EQ
                        then ar
                        else
                            if br /= EQ
                                then inv br
                                else inv xr
                    where
                        inv GT = LT
                        inv EQ = EQ
                        inv LT = GT
                        ar = a `compare` a'
                        br = b `compare` b'
                        xr = x `compare` x'
    let bnds = foldl' inj [] eqs
    let bndsm = M.fromList bnds
    test bndsm q

