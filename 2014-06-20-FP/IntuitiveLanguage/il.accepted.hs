import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Text.ParserCombinators.ReadP

type Val = [Rational]
data Op = Add | Sub | Mul | Div deriving (Show, Eq, Ord)
data Expr = Lit Rational | Var String | App Expr Expr | Op Op Expr Expr deriving (Show, Eq, Ord)
data Il = VarDef String Expr | FuncDef String [Expr] | Assgn [(String, Expr)] | Loop Expr Il | LoopN Integer Il | Print [Expr] deriving (Show, Eq, Ord)

ws = choice $ char `map` [' ', '\t', '\r', '\n']

wss = many1 ws

ows = many ws

alpha = choice $ char `map` ['a' .. 'z']

digit = choice $ char `map` ['0' .. '9']

alphanum = choice [alpha, digit]

natnum = many1 digit

name = do
    c <- alpha
    cs <- many alphanum
    ows
    let res = c : cs
    return res

xplit = do
    x <- natnum
    ows
    return (Lit ((read x)%1))

xnlit = do
    char '-'
    ows
    x <- natnum
    ows
    return (Lit ((-(read x))%1))

xlit = choice [xplit, xnlit]

xvar = do
    vn <- name
    return (Var vn)

apps f [] = f
apps f (x : xs) = apps (App f x) xs

xfunc = do
    fn <- name
    args <- many1 (between (char '[' >> ows) (char ']' >> ows) expr)
    return (apps (Var fn) args)

sumadd = do
    char '+'
    ows
    return (\x y -> Op Add x y)

sumsub = do
    char '-'
    ows
    return (\x y -> Op Sub x y)

sumop = choice [sumadd, sumsub]

prodmul = do
    char '*'
    ows
    return (\x y -> Op Mul x y)

proddiv = do
    char '/'
    ows
    return (\x y -> Op Div x y)

prodop = choice [prodmul, proddiv]

ppexpr = choice [xlit, xvar, xfunc, parexpr]

pexpr = ppexpr `chainl1` prodop

expr = pexpr `chainl1` sumop

parexpr = between (char '(' >> ows) (char ')' >> ows) expr

vardef = do
    vn <- name
    ws
    string "is"
    wss
    x <- expr
    char '.'
    return (VarDef vn x)
    
funcdef = do
    fn <- name
    ws
    string "is"
    wss
    string "function"
    wss
    string "of"
    wss
    arity <- natnum
    ows
    char ':'
    wss
    xs <- expr `sepBy` (char ',' >> ows)
    char '.'
    return (FuncDef fn xs)
    
letclause = do
    x <- expr
    ws
    string "to"
    wss
    v <- name
    return (v, x)

assgn = do
    string "assign"
    wss
    as <- letclause `sepBy` (ws >> string "and" >> wss)
    char '!'
    return (Assgn as)

loop = do
    string "do"
    ows
    char '{'
    ows
    x <- expr
    char '}'
    ows
    a <- assgn
    return (Loop x a)

prnt = do
    string "what"
    wss
    string "is"
    wss
    xs <- expr `sepBy` (ws >> string "and" >> wss)
    char '?'
    return (Print xs)

stmt = choice [vardef, funcdef, assgn, loop, prnt]

program = do
    ows
    prg <- stmt `sepBy` ows
    ows
    eof
    return prg

app xs x = init (tail xs) ++ [last xs + x * head xs]

eval _ (Lit x) = [x]
eval env (Var name) = fromJust $ name `M.lookup` env
eval env (App x1 x2) = (eval env x1) `app` (last $ eval env x2)
eval env (Op Add x1 x2) = [(last $ eval env x1) + (last $ eval env x2)]
eval env (Op Sub x1 x2) = [(last $ eval env x1) - (last $ eval env x2)]
eval env (Op Mul x1 x2) = [(last $ eval env x1) * (last $ eval env x2)]
eval env (Op Div x1 x2) = [(last $ eval env x1) / (last $ eval env x2)]

disp x = if denominator x == 1 then show (numerator x) else show (numerator x) ++ "/" ++ show (denominator x)

exec1 env (VarDef name x) = return (M.insert name (eval env x) env)
exec1 env (FuncDef name xs) = return (M.insert name ((last . (eval env)) `map` xs) env)
exec1 env (Assgn []) = return env
exec1 env (Assgn ((name, x) : rest)) = do
    env' <- exec1 env (VarDef name x)
    exec1 env' (Assgn rest)
exec1 env (Loop x xs) =
    let n = last $ eval env x
        in
            exec1 env (LoopN ((numerator n) `div` (denominator n)) xs)
exec1 env (LoopN 0 _) = return env
exec1 env (LoopN n x) = do
    env' <- exec1 env x
    exec1 env' (LoopN (pred n) x)
exec1 env (Print xs) = print `mapM_` xs >> return env
    where
        print x = putStrLn $ ", " `intercalate` (disp `map` (eval env x))

exec _ [] = return ()
exec env (x : xs) = do
    env' <- exec1 env x
    exec env' xs

main = do
    inp <- getContents
    let inp' = toLower `map` inp
    let parse = program `readP_to_S` inp'
    if null parse
        then putStrLn "syntax error"
        else do
            let code = fst . head $ parse
            exec M.empty code

