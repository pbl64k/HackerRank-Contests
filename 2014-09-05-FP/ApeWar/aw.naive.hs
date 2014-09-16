import Data.Array

inp = (read `map`) . words

cnt l r x = if l <= x && x <= r then 1 else 0

f acc t l r x y =
    if x == y
        then acc + cntx
        else
            if x > y
                then f (acc + cntx) t l r (t ! x) y
                else f (acc + cnty) t l r x (t ! y)
    where
        cntx = cnt l r x
        cnty = cnt l r y

tst 0 _ = return ()
tst m t = do
    qstr <- getLine
    let (x : y : l : r : _) = inp qstr
    putStrLn $ show $ f 0 t l r x y
    tst (pred m) t

main = do
    nmstr <- getLine
    let (n : m : _) = inp nmstr
    pstr <- getLine
    let t = listArray (1, n) (0 : (inp pstr))
    tst m t

