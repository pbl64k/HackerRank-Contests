import Data.Int
--import Text.ParserCombinators.ReadP
import Text.ParserCombinators.Parsec

--import System.IO.Unsafe

mmm = 1000000007

data Expr = Lit Int64 | Neg Expr | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr deriving Show

p_ws = choice (char `map` " \t\r")

p_ows = optional p_ws

--p_expr = choice [p_term, p_te]
p_expr = try p_te <|> p_term

p_te = do
    t <- p_term
    p_ows
    f <- choice [char '+' >> return Add, char '-' >> return Sub]
    x <- p_expr
    return (f t x)

--p_term = choice [p_factor, p_ft]
p_term = try p_ft <|> p_factor

p_ft = do
    f <- p_factor
    p_ows
    g <- choice [char '*' >> return Mul, char '/' >> return Div]
    t <- p_term
    return (g f t)

p_factor = choice [p_number, p_un, p_px]

p_digit = choice (char `map` "01234567890")

p_number = do
    p_ows
    nstr <- many1 p_digit
    return (Lit $ read nstr)

p_un = do
    p_ows
    g <- choice [char '+' >> return id, char '-' >> return Neg]
    f <- p_factor
    return (g f)

p_px = do
    p_ows
    char '('
    x <- p_expr
    p_ows
    char ')'
    return x

p_prg = do
    expr <- p_expr
    p_ows
    eof
    return expr

xx 1 x = x
xx p x =
    if p `mod` 2 == 0
        then xx (p `div` 2) ((x * x) `mod` mmm)
        else (x * xx (pred p) x) `mod` mmm

inv a = aux 0 mmm 1 a
    where
        aux t r newt 0 =
            --(unsafePerformIO $ putStrLn $ show (t, r, newt, 0)) `seq`
            if t < 0 then t + mmm else t
        aux t r newt newr =
            --(unsafePerformIO $ putStrLn $ show (t, r, newt, newr)) `seq`
            let q = r `div` newr
                in aux newt newr (t - q * newt) (r - q * newr)

eval (Lit x) = x `mod` mmm
eval (Neg x) = (-(eval x)) `mod` mmm
eval (Add x1 x2) = (eval x1 + eval x2) `mod` mmm
eval (Sub x1 x2) = (eval x1 - eval x2) `mod` mmm
eval (Mul x1 x2) = (eval x1 * eval x2) `mod` mmm
--eval (Div x1 x2) = (eval x1 * (inv $ eval x2)) `mod` mmm
eval (Div x1 x2) = (eval x1 * (xx (mmm - 2) $ eval x2)) `mod` mmm

main = do
    xstr <- getLine
    --let exprs = readP_to_S p_prg xstr
    let exprs = parse p_prg "(stdin)" xstr
    --putStrLn $ show exprs
    let (Right prg) = exprs
    putStrLn $ show $ eval prg

