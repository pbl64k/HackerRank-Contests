import Data.List
import Text.ParserCombinators.ReadP

data Lam = Var String | App Lam Lam | Lam String Lam deriving Show

lamWs = munch1 (`elem` " \t\r\n")

lamOws = optional lamWs

lamVar = do
        name <- munch1 (`elem` (concat [['a' .. 'z'], ['A' .. 'Z'], ['0' .. '9'], ['_']]))
        return (Var name)

lamApp = do
        char '('
        lamOws
        e1 <- lamExp
        lamWs
        e2 <- lamExp
        lamOws
        char ')'
        lamOws
        return (App e1 e2)

lamLam = do
        char '('
        lamOws
        char '\\'
        lamOws
        v <- lamVar
        vs <- many (lamWs >> lamVar)
        lamOws
        char '.'
        lamOws
        e <- lamExp
        lamOws
        char ')'
        lamOws
        return (foldr (\(Var v) e -> Lam v e) e (v : vs))

lamExp = choice [lamVar, lamApp, lamLam]

lamExpMain = do
        l <- lamExp
        eof
        return l

parseLambda = (((fst . head) .) . readP_to_S) lamExpMain

freeIn x (Var z) = x == z
freeIn x (App e1 e2) = x `freeIn` e1 || x `freeIn` e2
freeIn x (Lam z e) = x /= z && x `freeIn` e

t (Var x) = Var x
t (App e1 e2) = App (t e1) (t e2)
t (Lam x i@(Var z)) =
        if x == z
            then Var "I"
            else App (Var "K") (t i)
t (Lam x i@(Lam y e)) =
        if x `freeIn` i
            then t (Lam x (t i))
            else App (Var "K") (t i)
t (Lam x z@(App e1 e2)) =
        case e2 of
            (Var y) -> if x == y && (not $ x `freeIn` e1) then t e1 else tt
            _ -> tt
    where
        t0 True True = App (App (Var "S") (t (Lam x e1))) (t (Lam x e2))
        t0 True False = App (App (Var "C") (t (Lam x e1))) (t e2)
        t0 False True = App (App (Var "B") (t e1)) (t (Lam x e2))
        t0 False False = App (Var "K") (t z)
        tt = t0 (x `freeIn` e1) (x `freeIn` e2)

neat (Var x) = x
neat (App x (Var y)) = neat x ++ y
neat (App x y) = neat x ++ "(" ++ neat y ++ ")"

tst = do
        str <- getLine
        (putStrLn . neat . t . parseLambda) str

main = do
        tstr <- getLine
        mapM_ (const tst) [1 .. (read tstr)]

