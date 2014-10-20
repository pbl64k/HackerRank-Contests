{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Trans.State
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Sq
import qualified Data.Set as S
import System.IO
import Text.Parsec
import Text.Parsec.Char

import System.IO.Unsafe

--dbg x = unsafePerformIO $ putStrLn x
dbg _ = ()

data Expr = Var String | Let String Expr Expr | Lam [String] Expr | App Expr [Expr] deriving Show

data Type = Int | Bool | FVar Int | Pair Type Type | List Type | Arr [Type] Type deriving (Eq, Show)

isLam (Arr _ _) = True
isLam _ = False

p_id = do
    inc <- choice [letter, char '_']
    rest <- many (choice [alphaNum, char '_'])
    return (inc : rest)

p_var = do
    v <- p_id
    apps <- many (between (char '(') (char ')') p_args)
    return $ foldl' (\f x -> App f x) (Var v) apps

p_let = do
    string "let "
    v <- p_id
    string " = "
    e1 <- p_expr
    string " in "
    e2 <- p_expr
    return (Let v e1 e2)

p_lam = do
    vs <- between (string "fun ") (string "-> ") p_params
    e <- p_expr
    return (Lam vs e)

p_par = between (char '(') (char ')') p_expr

p_app = do
    f <- try p_par <|> p_var
    es <- many1 $ between (char '(') (char ')') p_args
    return $ foldl' (\f e -> App f e) f es

p_expr = try p_par <|> try p_lam <|> try p_let <|> try p_var <|> p_app

p_prg = do
    e <- p_expr
    eof
    return e

p_params = p_id `sepEndBy` char ' '

p_args = p_expr `sepBy` string ", "

env_vt = [
    --("fix", Arr [Arr [FVar 0] (FVar 0)] (FVar 0)),
    ("head", Arr [List (FVar 0)] (FVar 0)),
    ("tail", Arr [List (FVar 0)] (List (FVar 0))),
    ("nil", List (FVar 0)),
    ("cons", Arr [FVar 0, List (FVar 0)] (List (FVar 0))),
    ("cons_curry", Arr [FVar 0] (Arr [List (FVar 0)] (List (FVar 0)))),
    ("map", Arr [Arr [FVar 0] (FVar 1), List (FVar 0)] (List (FVar 1))),
    ("map_curry", Arr [Arr [FVar 0] (FVar 1)] (Arr [List (FVar 0)] (List (FVar 1)))),
    ("one", Int),
    ("zero", Int),
    ("succ", Arr [Int] Int),
    ("plus", Arr [Int, Int] Int),
    ("eq", Arr [FVar 0, FVar 0] Bool),
    ("eq_curry", Arr [FVar 0] (Arr [FVar 0] Bool)),
    ("not", Arr [Bool] Bool),
    ("true", Bool),
    ("false", Bool),
    ("pair", Arr [FVar 0, FVar 1] (Pair (FVar 0) (FVar 1))),
    ("pair_curry", Arr [FVar 0] (Arr [FVar 1] (Pair (FVar 0) (FVar 1)))),
    ("first", Arr [Pair (FVar 0) (FVar 1)] (FVar 0)),
    ("second", Arr [Pair (FVar 0) (FVar 1)] (FVar 1)),
    ("id", Arr [FVar 0] (FVar 0)),
    ("const", Arr [FVar 0] (Arr [FVar 1] (FVar 0))),
    ("apply", Arr [Arr [FVar 0] (FVar 1), FVar 0] (FVar 1)),
    ("apply_curry", Arr [Arr [FVar 0] (FVar 1)] (Arr [FVar 0] (FVar 1))),
    ("choose", Arr [FVar 0, FVar 0] (FVar 0)),
    ("choose_curry", Arr [FVar 0] (Arr [FVar 0] (FVar 0)))
    ]

data Env = Env { vt :: M.Map String Int,
    vt_st :: [M.Map String Int],
    st :: Sq.Seq Type,
    ut :: S.Set Int } deriving Show

add_t0 u = do
    env <- get
    let n = Sq.length $ st env
    let st' = st env Sq.|> (FVar n)
    let ut' = if u then n `S.insert` ut env else ut env
    let env' = Env { vt = vt env, vt_st = vt_st env, st = st', ut = ut' }
    put env'
    return n

add_ct0 u t = do
    n <- add_t0 u
    replace n t
    env <- get
    return n

add_t u v = do
    n <- add_t0 u
    env <- get
    let vt' = M.insert v n (vt env)
    let env' = Env { vt = vt', vt_st = vt_st env, st = st env, ut = ut env }
    put env'
    return n

add_ct u v t = do
    n <- add_t u v
    replace n t
    return n

get_t v = do
    env <- get
    let n = fromJust $ v `M.lookup` vt env
    get_ct n

get_nnct n = do
    env <- get
    let t = st env `Sq.index` n
    return t

get_ct n = do
    t <- get_nnct n
    norm t

giarr acc m [] = return (reverse acc, m)
giarr acc m (t : ts) = do
    (t', m') <- gi m t
    giarr (t' : acc) m' ts

gi m (FVar n) = do
    if n `M.member` m
        then return (FVar $ fromJust $ n `M.lookup` m, m)
        else do
            n' <- add_t0 True
            return (FVar n', M.insert n n' m)
gi m (Pair t1 t2) = do
    (t', m') <- gi m t1
    (t'', m'') <- gi m' t2
    return (Pair t' t'', m'')
gi m (List t) = do
    (t', m') <- gi m t
    return (List t', m')
gi m (Arr ts tr) = do
    (ts', m') <- giarr [] m ts
    (tr', m'') <- gi m' tr
    return (Arr ts' tr', m'')
gi m t = return (t, m)

glob_inst v t = do
    (t', _) <- gi M.empty t
    add_ct True v t'

norm (FVar n) = do
    t <- get_nnct n
    if t == FVar n
        then return t
        else norm t
norm (Pair t1 t2) = do
    t1' <- norm t1
    t2' <- norm t2
    return $ Pair t1' t2'
norm (List t) = do
    t' <- norm t
    return $ List t'
norm (Arr ts tr) = do
    ts' <- mapM norm ts
    tr' <- norm tr
    return $ Arr ts' tr'
norm t = return t

push_vt = do
    env <- get
    let env' = Env { vt = vt env, vt_st = vt env : vt_st env, st = st env, ut = ut env }
    put env'

pop_vt = do
    env <- get
    let (vt' : vt_st') = vt_st env
    let env' = Env { vt = vt', vt_st = vt_st', st = st env, ut = ut env }
    put env'

with_new_vt f = do
    push_vt
    x <- f
    pop_vt
    return x

instarr u acc m [] = return (reverse acc, m)
instarr u acc m (t : ts) = do
    (t', m') <- instantiate u m t
    instarr u (t' : acc) m' ts

instantiate u m (FVar v) = do
    env <- get
    if v `S.member` ut env
        then
            if v `M.member` m
                then {- (dbg "wth") `seq` -} return (FVar $ fromJust $ v `M.lookup` m, m)
                else do
                    n <- add_t0 u
                    {- (dbg $ "inst " ++ show v ++ " as " ++ show (FVar n)) `seq` -}
                    return (FVar n, M.insert v n m)
        else return (FVar v, m)
instantiate u m (Pair t1 t2) = do
    (t', m') <- instantiate u m t1
    (t'', m'') <- instantiate u m' t2
    return (Pair t' t'', m'')
instantiate u m (List t) = do
    (t', m') <- instantiate u m t
    return (List t', m')
instantiate u m (Arr ts tr) = do
    (ts', m') <- {- (dbg $ "..." ++ show (Arr ts tr)) `seq` -} instarr u [] m ts
    (tr', m'') <- instantiate u m' tr
    return (Arr ts' tr', m'')
instantiate u m t = return (t, m)

replace x t = do
    env <- get
    let st' = Sq.update x t $ st env
    let env' = Env { vt = vt env, vt_st = vt_st env, st = st', ut = ut env }
    {- (dbg $ show x ++ " " ++ show t) `seq` (dbg $ show env) `seq` (dbg $ show env') `seq` dbg "" `seq` -}
    put env'

--occurs x (FVar z) = do
--    if x == z
--        then return True
--        else do
--            t' <- get_nnct z
--            if t' == FVar z
--                then return False
--                else occurs x t'
--occurs x (Pair t1 t2) = do
--    b1 <- occurs x t1
--    b2 <- occurs x t2
--    return (b1 || b2)
--occurs x (List t) = occurs x t
--occurs x (Arr ts tr) = do
--    bs <- mapM (occurs x) ts
--    b <- occurs x tr
--    return (any id bs || b)
--occurs _ t = return False

--unify (FVar x) t = do
--    b <- occurs x t
--    if b
--        then error $ "Occurs check failed: " ++ show (FVar x) ++ " and " ++ show t
--        else nunify (FVar x) t
--unify t (FVar x) = do
--    b <- occurs x t
--    if b
--        then error $ "Occurs check failed: " ++ show (FVar x) ++ " and " ++ show t
--        else nunify (FVar x) t
--unify t1 t2 = nunify t1 t2
unify = nunify

nunify t1 t2 = do
    t1' <- norm t1
    t2' <- norm t2
    unify' t1' t2'    

unify' Int Int = return ()
unify' Bool Bool = return ()
unify' (FVar x) (FVar y) = do
    tx <- get_ct x
    ty <- get_ct y
    if tx == FVar x && ty == FVar y
        then replace x (FVar y)
        else unify tx ty
unify' (FVar x) t = do
    t' <- get_ct x
    {- (dbg $ show (t, t')) `seq` -}
    if t' == FVar x
        then replace x t
        else unify t' t
unify' t (FVar x) = do
    t' <- get_ct x
    {- (dbg $ show (t, t')) `seq` -}
    if t' == FVar x
        then replace x t
        else unify t t'
unify' (Pair t1 t2) (Pair t3 t4) = do
    unify t1 t3
    unify t2 t4
unify' (List t1) (List t2) = unify t1 t2
unify' (Arr ts1 tr1) (Arr ts2 tr2) = do
    if length ts1 == length ts2
        then do
            mapM_ (uncurry unify) (ts1 `zip` ts2)
            unify tr1 tr2
        else error $ "Unable to unify types " ++ show (Arr ts1 tr1) ++ " and " ++ show (Arr ts2 tr2)
unify' t1 t2 = error $ "Unable to unify types " ++ show t1 ++ " and " ++ show t2

inferr x = do
    r <- infer False x
    env <- get
    {- dbg (show env) `seq` -}
    get_ct r

--infer = infer'
infer u x = do
    env <- get
    r <- {- dbg ("Expr: " ++ show x) `seq` dbg (show env) `seq` dbg "" `seq` -} infer' u x
    env' <- get
    {- dbg ("Type for " ++ show x ++ " is: " ++ show (st env' `Sq.index` r)) `seq` dbg (show env') `seq` dbg "" `seq` -}
    return r

infer' u (Var v) = do
    t <- {- (dbg $ v) `seq` -} get_t v
    x <- instantiate u M.empty t
    n <- {- (dbg $ "adding...") `seq` -} add_ct0 u $ fst x
    env <- get
    (dbg $ show $ ut env) `seq` (dbg $ show (v, t, fst x)) `seq` dbg "" `seq` return n
infer' u (Let v e1 e2) = do
    n1 <- infer True e1
    t1 <- get_ct n1
    with_new_vt $ do
        --add_ct v (quantify t1)
        add_ct True v t1
        n2 <- infer u e2
        return n2
infer' u (Lam vs e) = do
    with_new_vt $ do
        ts <- mapM (add_t u) vs
        tr <- add_t0 u
        fn <- add_ct0 u (Arr ((FVar) `map` ts) (FVar tr))
        -- ??
        n <- infer u e
        t <- get_ct n
        unify (FVar tr) t
        return fn
infer' u (App f es) = do
    nf <- infer u f
    tf <- get_ct nf
    ns <- mapM (infer u) es
    ts <- mapM get_ct ns
    nr <- add_t0 u
    unify (Arr ts (FVar nr)) tf
    return nr

global = do
    mapM_ (\(v, t) -> glob_inst v t) env_vt

disp c m Int = ("int", c, m)
disp c m Bool = ("bool", c, m)
disp c m (FVar x) =
    if x `M.member` m
        then ([fromJust $ x `M.lookup` m], c, m)
        else ([c], succ c, M.insert x c m)
disp c m (Pair t1 t2) = ("pair[" ++ str1 ++ ", " ++ str2 ++ "]", c'', m'')
    where
        (str1, c', m') = disp c m t1
        (str2, c'', m'') = disp c' m' t2
disp c m (List t) = ("list[" ++ str ++ "]", c', m')
    where
        (str, c', m') = disp c m t
disp c m (Arr ts tr) = (
    (if null ts
        then "()"
        else
            if (null $ tail ts) && (not $ isLam $ head ts)
                then head params
                else "(" ++ intercalate ", " (reverse params) ++ ")")
    ++ " -> " ++ str, c'', m'')
    where
        f (acc, c, m) t = (str : acc, c', m')
            where
                (str, c', m') = disp c m t
        (params, c', m') = foldl' f ([], c, m) ts
        (str, c'', m'') = disp c' m' tr

display t = (if c > 'a' then "forall[" ++ intercalate " " ((: []) `map` ['a' .. pred c]) ++ "] " else "") ++ str
    where
        (str, c, m) = disp 'a' M.empty t

tst = do
    b <- isEOF
    if b
        then return ()
        else do
            x <- getLine
            let p = parse p_prg "(stdin)" x
            --putStrLn $ show p
            let (Right expr) = p
            let s = execState global $ Env { vt = M.empty, vt_st = [], st = Sq.empty, ut = S.empty }
            let t = evalState (inferr expr) s
            --putStrLn $ show t
            putStrLn $ display t
            tst

main = tst

