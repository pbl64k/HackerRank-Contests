import Control.Arrow
import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

data SimpleProposition = Name String | Var String | Rel String [SimpleProposition] deriving (Show, Eq, Ord)
data Proposition = Simple SimpleProposition | Eq SimpleProposition SimpleProposition | Neq SimpleProposition SimpleProposition deriving (Show, Eq, Ord)
data Rule = EmptyRule | Rule SimpleProposition [Proposition] deriving (Show, Eq, Ord)

type VarSet = S.Set String
type Env = M.Map String SimpleProposition
type Diseq = [(SimpleProposition, SimpleProposition)]

data Interpretation = Interpretation Integer Env Diseq deriving (Show, Eq, Ord)

type Knowledge = [Rule]
type Goal = [Proposition]
type Interpretations = [Interpretation]
type State = (Interpretation, Goal)
type States = [State]

data Command = Quit | New | Rules | Help deriving (Show, Eq, Ord)

data Op = OpRule Rule | OpQuery Goal | OpCommand Command | NoOp deriving (Show, Eq, Ord)

f `pmap` (Simple x) = Simple (f x)
f `pmap` (Eq x y) = Eq (f x) (f y)
f `pmap` (Neq x y) = Neq (f x) (f y)

pfold f i (Simple x) = f i x
pfold f i (Eq x y) = f (f i x) y
pfold f i (Neq x y) = f (f i x) y

unionmap fold f = fold (\s p -> s `S.union` f p) S.empty

zipTerms f xs ys res = foldl' (\r (a, b) -> r >>= f a b) [res] (zip xs ys)

vars'' :: SimpleProposition -> VarSet
vars'' (Var x) = S.singleton x
vars'' (Rel _ ps) = unionmap foldl' vars'' ps
vars'' _ = S.empty

vars' :: Proposition -> VarSet
vars' = unionmap pfold vars''

vars :: [Proposition] -> VarSet
vars = unionmap foldl' vars'

selfAssignment :: VarSet -> Env
selfAssignment = M.fromList . (map (id &&& Var)) . S.toList

emptyInterpretation :: VarSet -> Interpretation
emptyInterpretation vs = Interpretation 0 (selfAssignment vs) []

next :: Interpretation -> Interpretation
next (Interpretation step assgn diseq) = Interpretation (succ step) assgn diseq

stepOf :: Interpretation -> Integer
stepOf (Interpretation step _ _) = step

unbound :: Interpretation -> [Proposition]
unbound (Interpretation _ assgn _) = (M.elems assgn) >>= free
    where
        free x@(Var _) = [Simple x]
        free (Rel _ ps) = ps >>= free
        free _ = []

prepend' :: String -> SimpleProposition -> SimpleProposition
prepend' s (Var n) = Var (s ++ n)
prepend' s (Rel a ps) = Rel a ((prepend' s) `map` ps)
prepend' _ x = x

prepend :: String -> Proposition -> Proposition
prepend = pmap . prepend'

rewrite :: Integer -> Rule -> Rule
rewrite n (Rule p acs) = Rule (tag `prepend'` p) ((prepend tag) `map` acs)
    where
        tag = ("#" ++ show n ++ "-")
rewrite _ x = x

substProp' :: SimpleProposition -> SimpleProposition -> SimpleProposition -> SimpleProposition
substProp' _ _ x@(Name _) = x
substProp' (Var a) b x@(Var c) = if a == c then b else x
substProp' a b (Rel c ps) = Rel c ((substProp' a b) `map` ps)

substProp :: SimpleProposition -> SimpleProposition -> Proposition -> Proposition
substProp = (pmap .) . substProp'

substAssgn :: SimpleProposition -> SimpleProposition -> Env -> Env
substAssgn = (M.map .) . substProp'

substDiseq :: SimpleProposition -> SimpleProposition -> Diseq -> Diseq
substDiseq a b = map (s *** s)
    where
        s = substProp' a b

subst :: SimpleProposition -> SimpleProposition -> State -> States
subst a b (Interpretation step assgn diseq, ps) = [(Interpretation step (substAssgn a b assgn) (substDiseq a b diseq), map (substProp a b) ps)]

occursIn :: String -> SimpleProposition -> Bool
a `occursIn` (Var b) = a == b
a `occursIn` (Rel _ ps) = any (a `occursIn`) ps
_ `occursIn` _ = False

equalize :: SimpleProposition -> SimpleProposition -> State -> States
equalize av@(Var a) bv@(Var b) res = if a == b then [res] else subst av bv res
equalize av@(Var a) bexp res = if a `occursIn` bexp then [] else subst av bexp res
equalize aexp bv@(Var _) res = equalize bv aexp res
equalize (Name a) (Name b) res = if a == b then [res] else []
equalize (Rel a xs) (Rel b ys) res = if a == b && length xs == length ys then zipTerms equalize xs ys res else []
equalize _ _ _ = []

disequalize :: SimpleProposition -> SimpleProposition -> State -> States
disequalize a b (Interpretation step assgn diseq, g) = [(Interpretation step assgn ((a, b) : diseq), g)]

unify :: SimpleProposition -> SimpleProposition -> State -> States
unify (Name a) (Name b) res = if a == b then [res] else []
unify (Rel s1 ps1) (Rel s2 ps2) res = if s1 == s2 && length ps1 == length ps2 then zipTerms unify ps1 ps2 res else []
unify av@(Var a) bv@(Var b) res@(int, goal) = if a == b then [res] else [(int, (Eq av bv) : goal)]
unify a@(Var _) b (int, goal) = [(int, (Eq a b) : goal)]
unify a b@(Var _) res = unify b a res
unify _ _ _ = []

matchRule :: SimpleProposition -> Rule -> State -> States
matchRule p (Rule ph ps) (int, g) = unify p ph (int, ps ++ g)

valid'' :: SimpleProposition -> SimpleProposition -> Bool
valid'' (Name a) (Name b) = a /= b
valid'' (Var a) (Var b) = a /= b
valid'' (Rel s1 ps1) (Rel s2 ps2) = s1 /= s2 || length ps1 /= length ps2 || any (uncurry valid'') (zip ps1 ps2)
valid'' _ _ = True

valid' :: Diseq -> Bool
valid' = all (uncurry valid'')

valid :: Interpretation -> Bool
valid (Interpretation _ _ diseq) = valid' diseq

normalize' :: State -> States
normalize' i@(int, g) = if valid int then [i] else []

normalize :: States -> States
normalize = (>>= normalize')

deduceRule :: State -> Rule -> States
deduceRule (int, (Eq a b : ps)) EmptyRule = equalize a b (next int, ps)
deduceRule (int, (Neq a b : ps)) EmptyRule = disequalize a b (next int, ps)
deduceRule _ EmptyRule = []
deduceRule (int, ((Simple prop) : ps)) r = matchRule prop r (next int, ps)
deduceRule _ _ = []

deduceStep :: Knowledge -> State -> States
deduceStep knowledge state = ((rewrite (stepOf $ fst state)) `map` knowledge) >>= (((normalize .) . deduceRule) state)

deduce' :: Knowledge -> State -> Interpretations
deduce' knowledge state = states' >>= depthFirstDeduce
    where
        states' = deduceStep knowledge state
        depthFirstDeduce (int, []) =
            let unb = unbound int
                in
                    if null unb
                        then [int]
                        else deduce' knowledge (int, unb)
        depthFirstDeduce substate = deduce' knowledge substate

initGoal goal = (emptyInterpretation (vars goal), goal)

deduce :: Knowledge -> Goal -> Interpretations
deduce knowledge goal = if null goal then [fst int] else deduce' knowledge int
    where
        int = initGoal goal

logSymInitChar = choice (char `map` join [['a' .. 'z'], ['A' .. 'Z']])

logSymChar = choice (logSymInitChar : (char `map` join [['0' .. '9'], ['-']]))

logSymbol = do
    c <- logSymInitChar
    cs <- many logSymChar
    return (c : cs)

logName = do
    n <- logSymbol
    return (Name n)

logVar = do
    char '#'
    n <- logSymbol
    return (Var n)

logRel = do
    char '['
    rn <- logSymbol
    string ": "
    xs <- logSExpList
    char ']'
    return (Rel rn xs)

logSExp = choice [logName, logVar, logRel]

logCSExpList0 = do
    string ", "
    x <- logSExp
    xs <- logSExpList0
    return (x : xs)

logSExpList0 = choice [logCSExpList0, return []]

logSExpList = do
    x <- logSExp
    xs <- logSExpList0
    return (x : xs)

logEq = do
    char '<'
    x1 <- logSExp
    string " = "
    x2 <- logSExp
    char '>'
    return (Eq x1 x2)

logNeq = do
    char '<'
    x1 <- logSExp
    string " /= "
    x2 <- logSExp
    char '>'
    return (Neq x1 x2)

logExp = choice [logSExp >>= (return . Simple), logEq, logNeq]

logFact = do
    x <- logSExp
    char '.'
    return (Rule x [])

logCExpList0 = do
    string ", "
    x <- logExp
    xs <- logExpList0
    return (x : xs)

logExpList0 = choice [logCExpList0, return []]

logExpList1 = do
    x <- logExp
    xs <- logExpList0
    return (x : xs)

logExpList = choice [logExpList1, return []]

logCRule = do
    string "{("
    acs <- logExpList
    string ") => "
    c <- logSExp
    string "}."
    return (Rule c acs)

logRule = choice [logFact, logCRule] >>= (return . OpRule)

logQuery = do
    char '('
    xs <- logExpList
    string ")?"
    return (OpQuery xs)

logQuit = do
    string "quit!"
    return (OpCommand Quit)

logNew = do
    string "new!"
    return (OpCommand New)

logRules = do
    string "rules!"
    return (OpCommand Rules)

logHelp = do
    string "help!"
    return (OpCommand Help)

logCommand = choice [logQuit, logNew, logRules, logHelp]

logComment = do
    char '%'
    munch (const True)
    return NoOp

logNoOp = choice [logComment, return NoOp]

logInput = do
    op <- choice [logRule, logQuery, logCommand, logNoOp]
    eof
    return op

disp (Name a) = a
disp (Var a) = "#" ++ a
disp (Rel r ps) = "[" ++ r ++ ": " ++ intercalate ", " (disp `map` ps) ++ "]"

dispProp (Simple p) = disp p
dispProp (Eq p1 p2) = "<" ++ disp p1 ++ " = " ++ disp p2 ++ ">"
dispProp (Neq p1 p2) = "<" ++ disp p1 ++ " /= " ++ disp p2 ++ ">"

dispRule EmptyRule = "Current rules:\n====="
dispRule (Rule r []) = disp r ++ "."
dispRule (Rule r ps) = "{(" ++ intercalate ", " (dispProp `map` ps) ++ ") => " ++ disp r ++ "}."

outInt (Interpretation _ assgn _) = do
    let ps = M.toAscList assgn
    if null ps
        then putStrLn "SAT"
        else do
            putStrLn "SAT:"
            putStrLn "====="
            (\(k, v) -> putStrLn ("#" ++ k ++ " := " ++ disp v)) `mapM_` ps
            return ()

prcQuery rules q = do
    let res = (reverse rules) `deduce` q
    if null res
        then putStrLn "UNSAT"
        else outInt `mapM_` res

prc rules (OpRule r) = do
    putStrLn "Ok."
    return (False, r : rules)
prc rules (OpQuery q) = do
    prcQuery rules q
    putStrLn "Ready."
    return (False, rules)
prc rules (OpCommand New) = do
    putStrLn "Ok."
    return (False, [EmptyRule])
prc rules (OpCommand Rules) = do
    (putStrLn . dispRule) `mapM_` (reverse rules)
    putStrLn "Ready."
    return (False, rules)
prc rules (OpCommand Help) = do
    putStrLn "WatLog interpreter help page"
    putStrLn ""
    putStrLn "=== Commands: =========="
    putStrLn " help!     displays this help page"
    putStrLn " quit!     terminates the interpreter"
    putStrLn " new!      removes all currently known rules from the knowledge base"
    putStrLn " rules!    displays the list of currently known inference rules"
    putStrLn "=== Basic Syntax: ======"
    putStrLn " Whitespace may be neither omitted nor added spuriously."
    putStrLn " Names must consist of alphanumeric characters and dashes."
    putStrLn " The first character in a name must be alphabetic."
    putStrLn " Names are case-sensitive."
    putStrLn " Examples:"
    putStrLn "    a"
    putStrLn "    X12"
    putStrLn "    symmetricRelation"
    putStrLn "    merge-sort-2"
    putStrLn " Hashmark # is used as a prefix to indicate a variable."
    putStrLn " Examples:"
    putStrLn "    #x"
    putStrLn "    #parent-node"
    putStrLn "    #T72"
    putStrLn " Relations have the form [name: (name | variable), ...]"
    putStrLn " Examples:"
    putStrLn "    [less-than: one, two]"
    putStrLn "    [clique: #Vertex-1, #Vertex-2, #Vertex-3]"
    putStrLn " Names, variables and relations are simple propositions."
    putStrLn " Equivalence assertion is written using the \"=\" character"
    putStrLn " between two simple propositions in angle brackets."
    putStrLn " Example:"
    putStrLn "    <[p: #x] = #y>"
    putStrLn " Similarly for non-equivalence assertion but using the \"/=\" sign."
    putStrLn " Example:"
    putStrLn "    <[p: #x] /= #y>"
    putStrLn " Simple propositions together with assertions form the set of propositions."
    putStrLn "=== Facts & Rules: ====="
    putStrLn " Facts are relations known to hold."
    putStrLn " Simply follow a relation by a dot to specify a fact."
    putStrLn " Variables in facts are considered to be universally quantified."
    putStrLn " Examples:"
    putStrLn "    [less-than: one, two]."
    putStrLn "    [superset-of: #X, empty-set]."
    putStrLn " Rules specify antecedents and a single consequent."
    putStrLn " See examples for syntax."
    putStrLn " Variables in rules are considered to be universally quantified."
    putStrLn " Examples:"
    putStrLn "    {([parent-of: #X, #Y], [ancestor-of: #Y, #Z]) => [ancestor-of: #X, #Z]}."
    putStrLn "    {([loves: #A, #B], [loves: #C, #B], <#A /= #B>) => [jealous-of: #A, #C]}."
    putStrLn " NOTE: facts are merely syntax sugar for rules of the form:"
    putStrLn "    {() => FACT}."
    putStrLn "=== Queries: ==========="
    putStrLn " Queries are lists of propositions in parentheses followed by a question mark."
    putStrLn " Variables in propositions are considered to be existentially quantified."
    putStrLn " Example:"
    putStrLn "    ([p: #FIRST, #SECOND, atomic], <#FIRST /= #SECOND>)?"
    putStrLn "========================"
    putStrLn "Ready."
    return (False, rules)
prc rules (OpCommand Quit) = do
    putStrLn "Bye."
    return (True, rules)
prc rules _ = do
    return (False, rules)

tst rules = do
    l <- getLine
    let op = logInput `readP_to_S` l
    if null op
        then putStrLn "Syntax error. Type \"help!\" and press enter to see the help page." >> tst rules
        else do
            (quit, rules') <- prc rules ((fst . head) op)
            if quit then return () else tst rules'

main = tst [EmptyRule]

