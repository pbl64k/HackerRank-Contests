import Data.Array

kmp_table :: String -> Array Int Int
kmp_table (_ : []) = array (0, 0) [(0, -1)]
kmp_table (_ : _ : []) = array (0, 1) (zip [1 ..] [-1, 0])
kmp_table w = t
    where
        wl = length w
        warr = array (0, wl - 1) (zip [0 ..] w)
        tl = (zip [0 ..] (-1 : 0 : (kmp 2 0)))
        t = array (0, wl - 1) tl
        kmp pos cnd =
            if pos > wl - 1
                then []
                else
                    if (warr ! (pos - 1)) == (warr ! cnd)
                        then (succ cnd) : (kmp (succ pos) (succ cnd))
                        else
                            if cnd > 0
                                then kmp pos (snd (tl !! cnd))
                                else 0 : (kmp (succ pos) cnd)

kmp_search w s = s' 0 0
    where
        sl = length s
        wl = length w
        sarr = array (0, sl - 1) (zip [0 ..] s)
        warr = array (0, wl - 1) (zip [0 ..] w)
        t = kmp_table s
        s' m i =
            if m + i < sl
                then
                    if (warr ! i) == (sarr ! (m + i))
                        then
                            if i == wl - 1
                                then m
                                else s' m (succ i)
                        else
                            let m' = m + i - (t ! i)
                                in
                                    if (t ! i) > -1
                                        then s' m' (t ! i)
                                        else s' m' 0
                else sl

tst = do
    haystack <- getLine
    needle <- getLine
    putStrLn (if kmp_search needle haystack == length haystack then "NO" else "YES")

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. (read tstr)]

