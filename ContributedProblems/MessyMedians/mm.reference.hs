import qualified Data.Sequence as Sq

data Pheap a = PhEmpty (a -> a -> Bool) | Ph (a -> a -> Bool) Int a [Pheap a]

ph_empty f = PhEmpty f

ph_sing f e = Ph f 1 e []

ph_size (PhEmpty _) = 0
ph_size (Ph _ sz _ _) = sz

ph_isempty (PhEmpty _) = True
ph_isempty _ = False

ph_find (Ph _ _ e _) = e

ph_merge x (PhEmpty _) = x
ph_merge (PhEmpty _) x = x
ph_merge x@(Ph f asz a as) y@(Ph _ bsz b bs) = if f a b then Ph f (asz + bsz) a (y : as) else Ph f (asz + bsz) b (x : bs)

ph_ins (PhEmpty f) e = ph_sing f e
ph_ins x@(Ph f _ _ _) e = x `ph_merge` (ph_sing f e)

ph_del (Ph f _ _ xs) = ph_mp f xs

ph_mp f [] = ph_empty f
ph_mp _ [x] = x
ph_mp f (x : y : rest) = (x `ph_merge` y) `ph_merge` (ph_mp f rest)

med_mk = (ph_empty (>), ph_empty (<))

med_add (l, r) e = (l'', r'')
    where
        (l', r') = if ph_isempty l || e <= ph_find l then (ph_ins l e, r) else (l, ph_ins r e)
        (l'', r'') =
            if ph_size l' > succ (ph_size r')
                then (ph_del l', ph_ins r' (ph_find l'))
                else
                    if ph_size r' > ph_size l'
                        then (ph_ins l' (ph_find r'), ph_del r')
                        else (l', r')

med (l, _) = ph_find l

tst n t sq md = if n == t then return () else tst'
    where
        tst' = do
            xstr <- getLine
            let x = read xstr
            let md' = if x < 0 then sq `Sq.index` (n + x) else med_add md x
            putStrLn $ show (med md')
            tst (succ n) t (sq Sq.|> md') md'

main = do
    tstr <- getLine
    tst 0 (read tstr) Sq.empty med_mk

