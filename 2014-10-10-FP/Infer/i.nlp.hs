{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Trans.State
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Sq
import Text.Parsec
import Text.Parsec.Char

import System.IO.Unsafe

--dbg x = unsafePerformIO $ putStrLn x
dbg _ = ()

data Expr = Var String | Let String Expr Expr | Lam [String] Expr | App Expr [Expr] deriving Show

data Type = Int | Bool | UVar Int | FVar Int | Pair Type Type | List Type | Arr [Type] Type deriving (Eq, Show)

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

-- TODO: general app doesn't work: 'id()'
p_expr = try p_par <|> try p_lam <|> try p_let <|> try p_var <|> p_app

p_prg = do
    e <- p_expr
    eof
    return e

p_params = p_id `sepEndBy` char ' '

p_args = p_expr `sepBy` string ", "

env_vt = [
    ("head", Arr [List (UVar 0)] (UVar 0)),
    ("tail", Arr [List (UVar 0)] (List (UVar 0))),
    ("nil", List (UVar 0)),
    ("cons", Arr [UVar 0, List (UVar 0)] (List (UVar 0))),
    ("cons_curry", Arr [UVar 0] (Arr [List (UVar 0)] (List (UVar 0)))),
    ("map", Arr [Arr [UVar 0] (UVar 1), List (UVar 0)] (List (UVar 1))),
    ("map_curry", Arr [Arr [UVar 0] (UVar 1)] (Arr [List (UVar 0)] (List (UVar 1)))),
    ("one", Int),
    ("zero", Int),
    ("succ", Arr [Int] Int),
    ("plus", Arr [Int, Int] Int),
    ("eq", Arr [UVar 0, UVar 0] Bool),
    ("eq_curry", Arr [UVar 0] (Arr [UVar 0] Bool)),
    ("not", Arr [Bool] Bool),
    ("true", Bool),
    ("false", Bool),
    ("pair", Arr [UVar 0, UVar 1] (Pair (UVar 0) (UVar 1))),
    ("pair_curry", Arr [UVar 0] (Arr [UVar 1] (Pair (UVar 0) (UVar 1)))),
    ("first", Arr [Pair (UVar 0) (UVar 1)] (UVar 0)),
    ("second", Arr [Pair (UVar 0) (UVar 1)] (UVar 1)),
    ("id", Arr [UVar 0] (UVar 0)),
    ("const", Arr [UVar 0] (Arr [UVar 1] (UVar 0))),
    ("apply", Arr [Arr [UVar 0] (UVar 1), UVar 0] (UVar 1)),
    ("apply_curry", Arr [Arr [UVar 0] (UVar 1)] (Arr [UVar 0] (UVar 1))),
    ("choose", Arr [UVar 0, UVar 0] (UVar 0)),
    ("choose_curry", Arr [UVar 0] (Arr [UVar 0] (UVar 0)))
    ]

-- for debugging

data Env = Env { vt :: M.Map String Int,
    vt_st :: [M.Map String Int],
    st :: Sq.Seq Type } deriving Show

add_t0 = do
    env <- get
    let n = Sq.length $ st env
    let st' = st env Sq.|> (FVar n)
    let env' = Env { vt = vt env, vt_st = vt_st env, st = st' }
    put env'
    return n

add_ct0 t = do
    n <- add_t0
    unify (FVar n) t
    env <- get
    return n

add_t v = do
    n <- add_t0
    env <- get
    let vt' = M.insert v n (vt env)
    let env' = Env { vt = vt', vt_st = vt_st env, st = st env }
    put env'
    return n

add_ct v t = do
    n <- add_t v
    unify (FVar n) t
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
    let env' = Env { vt = vt env, vt_st = vt env : vt_st env, st = st env }
    put env'

pop_vt = do
    env <- get
    let (vt' : vt_st') = vt_st env
    let env' = Env { vt = vt', vt_st = vt_st', st = st env }
    put env'

with_new_vt f = do
    push_vt
    x <- f
    pop_vt
    return x

instarr acc m [] = return (reverse acc, m)
instarr acc m (t : ts) = do
    (t', m') <- instantiate m t
    instarr (t' : acc) m' ts

instantiate m (UVar v) = do
    if v `M.member` m
        then return (FVar $ fromJust $ v `M.lookup` m, m)
        else do
            n <- add_t0
            return (FVar n, M.insert v n m)
instantiate m (Pair t1 t2) = do
    (t', m') <- instantiate m t1
    (t'', m'') <- instantiate m' t2
    return (Pair t' t'', m'')
instantiate m (List t) = do
    (t', m') <- instantiate m t
    return (List t', m')
instantiate m (Arr ts tr) = do
    (ts', m') <- instarr [] m ts
    (tr', m'') <- instantiate m' tr
    return (Arr ts' tr', m'')
instantiate m t = return (t, m)

q n m (FVar v) =
    if v `M.member` m
        then (UVar $ fromJust $ v `M.lookup` m, n, m)
        else (UVar n, succ n, M.insert v n m)
q n m (Pair t1 t2) = (Pair t' t'', n'', m'')
    where
        (t', n', m') = q n m t1
        (t'', n'', m'') = q n' m' t2
q n m (List t) = (List t', n', m')
    where
        (t', n', m') = q n m t
q n m (Arr ts tr) = (Arr (reverse ts') tr', n'', m'')
    where
        f (ts, n, m) t = (t' : ts, n', m')
            where
                (t', n', m') = q n m t
        (ts', n', m') = foldl' f ([], n, m) ts
        (tr', n'', m'') = q n' m' tr
q n m t = (t, n, m)

quantify t = t'
    where
        (t', _, _) = q 0 M.empty t

--data Expr = Var String | Let String Expr Expr | Lam [String] Expr | App Expr [Expr] deriving Show
--data Type = Int | Bool | UVar Int | FVar Int | Pair Type Type | List Type | Arr [Type] Type deriving Show

--repl x t v@(FVar z) = if x == z then t else v
--repl x t (Pair t1 t2) = Pair (repl x t t1) (repl x t t2)
--repl x t (List t1) = List $ repl x t t1
--repl x t (Arr ts tr) = Arr (repl x t `map` ts) (repl x t tr)
--repl _ _ t = t
--
--replace x t = do
--    env <- get
--    let st' = repl x t `fmap` st env
--    let env' = Env { vt = vt env, vt_st = vt_st env, st = st' }
--    (dbg $ show x ++ " " ++ show t) `seq` (dbg $ show env) `seq` (dbg $ show env') `seq` dbg "" `seq` put env'

replace x t = do
    env <- get
    let st' = Sq.update x t $ st env
    let env' = Env { vt = vt env, vt_st = vt_st env, st = st' }
    (dbg $ show x ++ " " ++ show t) `seq` (dbg $ show env) `seq` (dbg $ show env') `seq` dbg "" `seq` put env'

occurs x (FVar z) = do
    if x == z
        then return True
        else do
            t' <- get_nnct z
            if t' == FVar z
                then return False
                else occurs x t'
occurs x (Pair t1 t2) = do
    b1 <- occurs x t1
    b2 <- occurs x t2
    return (b1 || b2)
occurs x (List t) = occurs x t
occurs x (Arr ts tr) = do
    bs <- mapM (occurs x) ts
    b <- occurs x tr
    return (any id bs || b)
occurs _ t = return False

unify t1 t2 = do
    t1' <- norm t1
    t2' <- norm t2
    unify' t1' t2'    

--unify (UVar _) _ = return ()
--unify _ (UVar _) = return ()
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
    (dbg $ show (t, t')) `seq` if t' == FVar x
        then do
            b <- occurs x t
            if b
                then error $ "Occurs check failed: " ++ show (FVar x) ++ " and " ++ show t
                else replace x t
        else unify t' t
unify' t (FVar x) = do
    t' <- get_ct x
    (dbg $ show (t, t')) `seq` if t' == FVar x
        then do
            b <- occurs x t
            if b
                then replace x t
                else replace x t
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
    r <- infer x
    env <- get
    dbg (show env) `seq` get_ct r

--infer = infer'
infer x = do
    env <- get
    r <- dbg ("Expr: " ++ show x) `seq` dbg (show env) `seq` dbg "" `seq` infer' x
    env' <- get
    dbg ("Type for " ++ show x ++ " is: " ++ show (st env' `Sq.index` r)) `seq` dbg (show env') `seq` dbg "" `seq` return r

infer' (Var v) = do
    t <- get_t v
    x <- instantiate M.empty t
    n <- add_ct0 $ fst x
    return n
infer' (Let v e1 e2) = do
    n1 <- infer e1
    t1 <- get_ct n1
    with_new_vt $ do
        --add_ct v (quantify t1)
        add_ct v t1
        n2 <- infer e2
        return n2
infer' (Lam vs e) = do
    with_new_vt $ do
        ts <- mapM add_t vs
        tr <- add_t0
        fn <- add_ct0 (Arr (FVar `map` ts) (FVar tr))
        n <- infer e
        t <- get_ct n
        unify (FVar tr) t
        return fn
infer' (App f es) = do
    nf <- infer f
    tf <- get_ct nf
    ns <- mapM infer es
    ts <- mapM get_ct ns
    nr <- add_t0
    unify (Arr ts (FVar nr)) tf
    return nr

global = do
    mapM_ (\(v, t) -> add_ct v t) env_vt

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

main = do
    x <- getLine
    let p = parse p_prg "(stdin)" x
    --putStrLn $ show p
    let (Right expr) = p
    let s = execState global $ Env { vt = M.empty, vt_st = [], st = Sq.empty }
    let t = evalState (inferr expr) s
    --putStrLn $ show t
    putStrLn $ display t

