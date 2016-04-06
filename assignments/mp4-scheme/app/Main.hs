module Main where

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import qualified Data.HashMap.Strict as H

fixMe = undefined


-- Datatypes
-- ---------

-- Exp data type (hold parse results before evaluation)
data Exp = IntExp Integer
         | SymExp String
         | SExp [Exp]
         deriving (Show)

-- Val data type (for results of evaluation)
data Val = IntVal Integer
         | SymVal String
         | ExnVal String
         | PrimVal ([Val] -> Val)
         | Closure [String] Exp Env
         | DefVal String Val
         | ConsVal Val Val
         | Macro [String] Exp Env

instance Eq Val where
    (IntVal i1) == (IntVal i2) = (i1 == i2)
    (SymVal s1) == (SymVal s2) = (s1 == s2)
    _ == _ = False

-- Parsers
-- -------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- Lexicals
symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit

-- Remember to define this parser. You should handle spaces, tabs, and newlines.
whitespace :: Parser String
whitespace = many aux
               where aux = space
                         <|> tab
                         <|> newline

identFirst :: Parser Char
identFirst = oneOf "-*+/:'?><=!,"
         <|> oneOf ['a'..'z']
         <|> oneOf ['A'..'Z']
identRest :: Parser Char
identRest = identFirst
         <|> adigit
identifier :: Parser String
identifier = do f <- identFirst
                r <- many identRest
                whitespace
                return $ f:r

                
-- Grammaticals

anInt :: Parser Exp
anInt = do  d <- digits
            return $ IntExp (read d)

aSym :: Parser Exp
aSym = do s <- identifier
          return $ SymExp s

aForm :: Parser Exp
aForm = do symbol "("
           exps <- anExp `sepBy` whitespace
           symbol ")"
           return $ SExp exps

aQuote :: Parser Exp
aQuote = do symbol "'"
            exp <- anExp
            return $ SExp [SymExp "quote", exp]

aQuasi :: Parser Exp
aQuasi = do symbol "`"
            exp <- anExp
            return $ SExp [SymExp "quasiquote", exp]

aUnquote :: Parser Exp
aUnquote = do symbol ","
              exp <- anExp
              return $ SExp [SymExp "unquote", exp]

-- Remember to modify `anExp` as to make it aware of Exp parsers you add
anExp :: Parser Exp
anExp = aQuote
    <|> aQuasi
    <|> aUnquote
    <|> anInt
    <|> aSym
    <|> aForm

-- Environment
-- -----------

-- Lift/Lower Haskell Bool to/from a boolean value in our Scheme representation
liftbool :: Bool -> Val
liftbool False = SymVal "nil"
liftbool True  = SymVal "t"

lowerbool :: Val -> Bool
lowerbool (SymVal "nil") = False
lowerbool _              = True

-- Lift/Lower a Haskell Int to/from an int value in our Scheme representation
liftint :: Integer -> Val
liftint = IntVal

lowerint :: Val -> Integer
lowerint (IntVal i) = i
lowerint _          = error "Cannot lower, not an IntVal!"

-- These are helpers which take a Haskell operator (like `(+)`), a base-case
-- (what's the base-case for `(+)`?), and turn them into a suitable `PrimVal` to
-- be used by our Scheme.
testPrimVal :: Val -> [Val] -> Val
testPrimVal = \(PrimVal f) vs ->f vs

liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> Val
liftIntOp f z = PrimVal (\inp -> 
    case inp of 
        x:xs -> liftint $ foldl1 f (map lowerint inp) 
        _    -> liftint z)

liftIntBoolOp :: (Integer -> Integer -> Bool) -> Val
liftIntBoolOp f = PrimVal (\inp -> 
    case (map lowerint inp) of 
        x:xs@(y:ys) -> liftbool (aux True f x xs)
                         where aux a f x (y:ys) = a && (f x y) && (aux a f y ys)
                               aux a f x [] = a
        _    -> liftbool True)

liftBoolOp :: (Bool -> Bool -> Bool) -> Val
liftBoolOp f = PrimVal (\inp -> 
    case (map lowerbool inp) of 
        xx@(x:xs) -> liftbool (aux True f xx)
                               where aux a f xx@(x:xs) = aux (f a x) f xs
                                     aux a f [] = a
        _    -> liftbool True)

liftOther :: String -> Val
liftOther s = case s of 
    "eq?" -> PrimVal (\inp -> 
        case inp of 
            xx@(x:xs) -> liftbool (aux x xs)
                               where aux a (x:xs) = (a == x) && (aux x xs)
                                     aux a [] = True
            _    -> liftbool True)
    "not" -> PrimVal (\inp -> 
        case (map lowerbool inp) of 
            [False] -> liftbool True
            [True] -> liftbool False
            _    -> ExnVal $ "Incorrect number of arguments. not form is unary.")

liftList :: [Val] -> Val
liftList (x:xs) = ConsVal x (liftList xs)
liftList [] = SymVal "nil"

lowerList :: Val -> [Val]
lowerList (ConsVal v1 v2) = v1:(lowerList v2)
lowerList (SymVal "nil") = []
lowerList v = [v]

liftCar :: Val
liftCar = PrimVal (\inp -> 
        case inp of 
            (ConsVal v1 v2):xs -> v1
            x:xs -> ExnVal $ "Not a cons cell: " ++ (show x))

liftCdr :: Val
liftCdr = PrimVal (\inp -> 
        case inp of 
            (ConsVal v1 v2):xs -> v2
            x:xs -> ExnVal $ "Not a cons cell: " ++ (show x))

-- Pretty name for the Env type
type Env = H.HashMap String Val

-- The `runtime` is the initial environment for evaluation. Primitive operators
-- such as "+", "eq?", and "cdr" must be inserted into this runtime.
runtime :: Env
runtime = H.fromList    [ ("+", liftIntOp (+) 0)
                        , ("-", liftIntOp (-) 0)
                        , ("*", liftIntOp (*) 1)    
                        , (">", liftIntBoolOp (>))                    
                        , ("<", liftIntBoolOp (<))                    
                        , (">=", liftIntBoolOp (>=))                    
                        , ("<=", liftIntBoolOp (<=))                    
                        , ("=", liftIntBoolOp (==))                    
                        , ("!=", liftIntBoolOp (/=))  
                        , ("and", liftBoolOp (&&))         
                        , ("or", liftBoolOp (||))      
                        , ("eq?", liftOther "eq?")
                        , ("not", liftOther "not")
                        , ("list", PrimVal liftList)
                        , ("car", liftCar)
                        , ("cdr", liftCdr)
                        ]

-- Evaluation
-- ----------

-- Helpers
quote :: Exp -> Val
quote (SymExp s) = SymVal s
quote (IntExp i) = IntVal i
quote (SExp xx) = liftList (map quote xx)

unquote :: Val -> Exp
unquote (SymVal s) = SymExp s
unquote (IntVal i) = IntExp i
unquote c = SExp (map unquote (lowerList c))


-- This `eval` must handle every way an `Exp` could be constructed.
eval :: Exp -> Env -> Val
eval (IntExp i) env                                    --- integers
    = IntVal i

eval (SymExp s) env =
      case (H.lookup s env) of
           Just v -> v
           Nothing -> ExnVal $ "Symbol " ++ s ++ " has no value"
           --Nothing -> SymVal s

eval (SExp []) env = SymVal "nil"
eval (SExp (x:xs)) env =              
       case x of
        SymExp "define" -> DefVal s1 v
                          where e1:e2:e3:[] = xs
                                SymExp s1 = e1
                                SExp yy = e2
                                v = Closure (map aux yy) e3 (H.insert s1 v env)
                                    where aux (SymExp s) = s
        SymExp "def" -> DefVal s1 v
                       where e1:e2:[] = xs
                             SymExp s1 = e1
                             v = eval e2 env
        SymExp "lambda" -> Closure (map aux yy) e2 env
                             where e1:e2:[]=xs
                                   SExp yy = e1
                                   aux (SymExp s) = s
        SymExp "quote" -> let (x:xx) = xs in (quote x)
        SymExp "cond" -> case xs of
            [SExp (x1:x2:xx)] -> if (eval x1 env == SymVal "t") 
                then (eval x2 env) else (eval (SExp (x:[SExp xx])) env)
            [SExp _] -> SymVal "nil"
        SymExp "let" ->  eval e env1
                          where [SExp xx, e] = xs
                                env1 = (H.union env2 env)
                                env2 = H.fromList (zip s1 v1)
                                s1 = map aux1 xx
                                aux1 (SExp [SymExp s,_]) = s
                                v1 = map (flip eval env) vv
                                vv = map aux2 xx
                                aux2 (SExp [_,e1]) = e1
        SymExp "cons" -> ConsVal (eval e1 env) (eval e2 env)
                         where [e1, e2] = xs
        SymExp "eval" -> let (x:xx) = xs; v = eval x env; e = unquote v in (eval e env)
        SymExp "quasiquote" -> quasiquote x1
                                 where x1 = head xs
                                       quasiquote (SExp [SymExp "unquote", e]) = eval e env
                                       quasiquote (SymExp s) = SymVal s
                                       quasiquote (IntExp i) = IntVal i
                                       quasiquote (SExp xx) = liftList (map quasiquote xx)
        SymExp "defmacro" -> DefVal s1 v
                          where e1:e2:e3:[] = xs
                                SymExp s1 = e1
                                SExp yy = e2
                                v = Macro (map aux yy) e3 (H.insert s1 v env)
                                    where aux (SymExp s) = s
        _ -> case (eval x env) of
              PrimVal f -> f (map (flip eval env) xs)
              Closure ss e env1 -> eval e (H.union (H.fromList 
                (zip ss (map (flip eval env) xs))) env1) 
              Macro ss e env1 -> eval e1 env
                                  where e1 = unquote v1
                                        v1 = eval e (H.union (H.fromList 
                                            (zip ss (map quote xs))) env1)                
              _ -> ExnVal ("Symbol " ++ (show x) ++ " has no value.")




-- Printing
-- --------

-- This `show` must handle every way a `Val` could be constructed.
instance Show Val where
    --show :: Val -> String
    show (IntVal i)         = show i
    show (SymVal s)         = s
    show (ExnVal s)         = "*** Scheme-Exception: " ++ s
    show (PrimVal f)        = "*primitive*"
    show (DefVal s v)       = s
    show (Closure s e env)  = "*closure*"
    show (ConsVal v1 v2)    = "(" ++ (showCons (ConsVal v1 v2)) ++ ")"
    show (Macro s e env)  = "*macro*"

-- helper
showCons :: Val -> String
showCons (ConsVal v1 v2) = case v2 of 
        ConsVal _ _ -> (show v1) ++ " " ++ (showCons v2)
        SymVal "nil" -> show v1 ++ " "
        _ -> (show v1) ++ " . " ++ (show v2)


-- REPL
-- ----

-- The line with `EVAL, PRINT` is where valid parses make it through to.
-- Remember to modify this so that the result of `eval` is checked. There is
-- special behavior in the REPL if the evaluation returns a `DefVal`
repl :: Env -> IO ()
repl env =
    do  putStr "scheme> "
        l <- getLine                                                        -- READ
        case parse anExp "Expression" l of                                  -- READ
            Right exp -> case (eval exp env) of 
                            DefVal s v -> do putStrLn $ show (eval exp env) 
                                             repl (H.insert s v env)
                            _ -> putStrLn $ show (eval exp env)             -- EVAL, PRINT
            Left pe   -> putStrLn (show pe)                                 -- PRINT
        repl env                                                            -- LOOP






