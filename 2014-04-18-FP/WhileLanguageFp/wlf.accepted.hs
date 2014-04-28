import Control.Monad
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import System.IO
import Text.ParserCombinators.ReadP

data Var = Var String deriving (Show, Eq)
data Numb = Numb Int64 deriving (Show, Eq)
data OpA = OpAdd | OpSub | OpMul | OpDiv deriving (Show, Eq)
data OpB = OpAnd | OpOr deriving (Show, Eq)
data OpR = OpGt | OpLt deriving (Show, Eq)
data AExp = ALit Numb | VarRef Var | AExp OpA AExp AExp deriving (Show, Eq)
data BExp = BLit Bool | BExp OpB BExp BExp | Cmp OpR AExp AExp deriving (Show, Eq)
data Stmt = Nop | Assign Var AExp | Seq Stmt Stmt | Cond BExp Stmt Stmt | Loop BExp Stmt deriving (Show, Eq)

w_ws = munch1 (\x -> (not (x >= 'a' && x <= 'z')) && (not (x >= '0' && x <= '9')) && (not (x `elem` "(){}:=;+-*/><")))

w_ows = optional w_ws

w_opar = char '(' >> w_ows >> return ()

w_cpar = char ')' >> w_ows >> return ()

w_var = do
        name <- munch1 (\x -> x >= 'a' && x <= 'z')
        w_ows
        return (Var name)

w_numb = do
        numstr <- munch1 (\x -> x >= '0' && x <= '9')
        w_ows
        (return . Numb . read) numstr

w_cop c cons = char c >> w_ows >> return cons

w_kwop kw cons = string kw >> w_ows >> return cons

w_add = w_cop '+' OpAdd

w_sub = w_cop '-' OpSub

w_mul = w_cop '*' OpMul

w_div = w_cop '/' OpDiv

w_and = w_kwop "and" OpAnd

w_or = w_kwop "or" OpOr

w_gt = w_cop '>' OpGt

w_lt = w_cop '<' OpLt

w_opr = choice [w_gt, w_lt]

w_ax_lit = do
        n <- w_numb
        return (ALit n)

w_ax_varref = do
        v <- w_var
        return (VarRef v)

w_ax_prd_par = between w_opar w_cpar w_ax_prd

w_ax_prd_rst' = do
        o <- choice [w_mul, w_div]
        b <- w_ax_prd_s
        f <- w_ax_prd_rst
        return (\x -> f (AExp o x b))

w_ax_prd_rst = choice [w_ax_prd_rst', return id]

w_ax_prd_s = choice [w_ax_lit, w_ax_varref, w_ax_sum_par]

w_ax_prd = do
        b <- w_ax_prd_s
        f <- w_ax_prd_rst
        return (f b)

w_ax_sum_par = between w_opar w_cpar w_ax_sum

w_ax_sum_rst' = do
        o <- choice [w_add, w_sub]
        b <- w_ax_sum_s
        f <- w_ax_sum_rst
        return (\x -> f (AExp o x b))

w_ax_sum_rst = choice [w_ax_sum_rst', return id]

w_ax_sum_s = choice [w_ax_prd, w_ax_sum_par]

w_ax_sum = do
        b <- w_ax_sum_s
        f <- w_ax_sum_rst
        return (f b)

w_ax = w_ax_sum

w_bx_true = string "true" >> w_ows >> return (BLit True)

w_bx_false = string "false" >> w_ows >> return (BLit False)

w_bx_opr = do
        a1 <- w_ax
        o <- w_opr
        a2 <- w_ax
        return (Cmp o a1 a2)

w_bx_conj_par = between w_opar w_cpar w_bx_conj

w_bx_conj_rst' = do
        o <- w_and
        b <- w_bx_conj_s
        f <- w_bx_conj_rst
        return (\x -> f (BExp o x b))

w_bx_conj_rst = choice [w_bx_conj_rst', return id]

w_bx_conj_s = choice [w_bx_true, w_bx_false, w_bx_opr, w_bx_disj_par]

w_bx_conj = do
        b <- w_bx_conj_s
        f <- w_bx_conj_rst
        return (f b)

w_bx_disj_par = between w_opar w_cpar w_bx_disj

w_bx_disj_rst' = do
        o <- w_or
        b <- w_bx_disj_s
        f <- w_bx_disj_rst
        return (\x -> f (BExp o x b))

w_bx_disj_rst = choice [w_bx_disj_rst', return id]

w_bx_disj_s = choice [w_bx_conj, w_bx_disj_par]

w_bx_disj = do
        b <- w_bx_disj_s
        f <- w_bx_disj_rst
        return (f b)

w_bx = w_bx_disj

w_s_assign = do
        v <- w_var
        string ":="
        w_ows
        a <- w_ax
        return (Assign v a)

w_s_rest = do
        char ';'
        w_ows
        s2 <- w_s
        return (Just s2)

w_s_seq' = do
        s1 <- choice [w_s_assign, w_s_if, w_s_loop]
        optional (char ';' >> w_ws)
        rest <- choice [w_s_rest, return Nothing]
        if rest == Nothing
            then return s1
            else return (Seq s1 (fromJust rest))

w_s_if = do
        string "if"
        w_ows
        b <- w_bx
        string "then"
        w_ows
        char '{'
        w_ows
        s1 <- w_s
        char '}'
        w_ows
        string "else"
        w_ows
        char '{'
        w_ows
        s2 <- w_s
        char '}'
        w_ows
        return (Cond b s1 s2)

w_s_loop = do
        string "while"
        w_ows
        b <- w_bx
        string "do"
        w_ows
        char '{'
        w_ows
        s <- w_s
        char '}'
        w_ows
        return (Loop b s)

w_s = w_s_seq'

w_p = do
        w_ows
        s <- w_s
        eof
        return s

lift2 eval op e1 e2 m = (eval e1 m) `op` (eval e2 m)

eval_x (ALit (Numb x)) _ = x
eval_x (VarRef (Var v)) m = fromMaybe 0 $ M.lookup v m
eval_x (AExp OpAdd a1 a2) m = lift2 eval_x (+) a1 a2 m
eval_x (AExp OpSub a1 a2) m = lift2 eval_x (-) a1 a2 m
eval_x (AExp OpMul a1 a2) m = lift2 eval_x (*) a1 a2 m
eval_x (AExp OpDiv a1 a2) m = lift2 eval_x div a1 a2 m

eval_b (BLit x) _ = x
eval_b (BExp OpAnd b1 b2) m = lift2 eval_b (&&) b1 b2 m
eval_b (BExp OpOr b1 b2) m = lift2 eval_b (||) b1 b2 m
eval_b (Cmp OpGt a1 a2) m = lift2 eval_x (>) a1 a2 m
eval_b (Cmp OpLt a1 a2) m = lift2 eval_x (<) a1 a2 m

eval_s Nop m = m
eval_s (Assign (Var v) x) m = M.insert v (eval_x x m) m
eval_s (Seq s1 s2) m = eval_s s2 (eval_s s1 m)
eval_s (Cond b s1 s2) m =
        if eval_b b m
            then eval_s s1 m
            else eval_s s2 m
eval_s ls@(Loop b s) m =
        if eval_b b m
            then eval_s (Seq s ls) m
            else m

main = do
        pstr <- hGetContents stdin
        let p = readP_to_S w_p pstr
        if null p
            then putStrLn "INVALID INPUT"
            else mapM_ (\(k, v) -> putStrLn (k ++ " " ++ (show v))) (sort $ M.toList (eval_s ((fst . head) p) M.empty))
    
