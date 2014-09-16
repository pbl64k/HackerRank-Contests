import Data.Array
import Data.List
import Text.ParserCombinators.ReadP

t_bit0 = char '.' >> return 0

t_bit1 = char 'X' >> return 1

t_bit = choice [t_bit0, t_bit1]

t_leaf n p = do
    bit <- t_bit
    return (succ n, [bit], [[p, 0, n, 0]])

t_node n p = do
    char '('
    (n', lt, ls) <- t_tree (succ n) n
    char ' '
    nbit <- t_bit
    char ' '
    (n'', rt, rs) <- t_tree n' n
    char ')'
    return (n'', [nbit] ++ lt ++ rt, [[p, succ n, n, n']] ++ ls ++ rs)

t_tree n p = choice [t_leaf n p, t_node n p]

t_fulltree = do
    t <- t_tree 1 0
    eof
    return t

0 `bits` _ = []
n `bits` x = (x `mod` 2) : ((pred n) `bits` (x `div` 2))

makerf rbits = rf
    where
        tbl = listArray (0, pred $ length rbits) rbits
        rf bs = tbl ! (foldl' (\acc b -> acc * 2 + b) 0 bs)

0 `guard` _ = ""
_ `guard` c = c

recover _ _ 0 = ""
recover st shape n = l `guard` "(" ++ recover st shape l ++ sp ++ c ++ sp ++ recover st shape r ++ l `guard` ")"
    where
        [p, l, s, r] = shape ! n
        sp = l `guard` " "
        c = if st ! n == 1 then "X" else "."

transition sz st shape rf = listArray (0, sz) (0 : ((rf . ((st !) `map`) . (shape !)) `map` [1 .. sz]))
    
query st shape n [] = if st ! n == 1 then "X" else "."
query st shape n ('<' : q) = query st shape l q
    where
        [p, l, s, r] = shape ! n
query st shape n ('>' : q) = query st shape r q
    where
        [p, l, s, r] = shape ! n

tst 0 _ _ _ = return ()
tst n life shape pos = do
    qstr <- getLine
    let (off : q : _) = words qstr
    let pos' = pos + read off
    let qe = init (tail q)
    putStrLn $ query (life !! pos') shape 1 qe
    tst (pred n) life shape pos'

main = do
    rule <- getLine >>= (return . read)
    let rbits = 16 `bits` rule
    let rf = makerf rbits
    init <- getLine
    let (sz, lstst, lstshape) = fst $ head $ readP_to_S t_fulltree init
    let st = (0, sz) `listArray` (0 : lstst)
    let shape = (1, sz) `listArray` (lstshape)
    --putStrLn $ recover st shape 1
    --putStrLn $ recover (transition sz st shape rf) shape 1
    --putStrLn $ recover (transition sz (transition sz st shape rf) shape rf) shape 1
    let life = st : [transition sz st' shape rf | st' <- life]
    n <- getLine >>= (return . read)
    tst n life shape 0

