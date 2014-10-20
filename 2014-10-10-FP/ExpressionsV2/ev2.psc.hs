import Data.Bits
import Data.Int
import Text.ParserCombinators.Parsec

mmm = 1000000007

p_ws = oneOf " \t\r"

p_ows = optional p_ws

p_expr = try p_te <|> p_term

p_te = do
    t <- p_term
    p_ows
    f <- choice [char '+' >> return plus, char '-' >> return minus]
    x <- p_expr
    return (f t x)

p_term = try p_ft <|> p_factor

p_ft = do
    f <- p_factor
    p_ows
    g <- choice [char '*' >> return mult, char '/' >> return divn]
    t <- p_term
    return (g f t)

p_factor = choice [p_number, p_un, p_px]

p_digit = oneOf "01234567890"

p_number = do
    p_ows
    nstr <- many1 p_digit
    return $ (read :: String -> Int64) nstr

p_un = do
    p_ows
    g <- choice [char '+' >> return id, char '-' >> return (\x -> (-x) `mod` mmm)]
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

-- acc := 1
mp acc _ 0 = acc
mp acc b e = mp (if odd e then ((acc * b) `mod` mmm) else acc) ((b * b) `mod` mmm) (shiftR e 1)

inv a = aux 0 mmm 1 a
    where
        aux t r newt 0 = if t < 0 then t + mmm else t
        aux t r newt newr =
            let q = r `div` newr
                in aux newt newr (t - q * newt) (r - q * newr)

plus a b = (a + b) `mod` mmm
minus a b = (a - b) `mod` mmm
mult a b = (a * b) `mod` mmm
--divn a b = (a * (inv b)) `mod` mmm
divn a b = (a * xx (mmm - 2) b) `mod` mmm

main = do
    xstr <- getLine
    let exprs = parse p_prg "(stdin)" xstr
    let (Right prg) = exprs
    putStrLn $ show prg

