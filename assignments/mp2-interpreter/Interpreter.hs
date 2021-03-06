{-----------------------------------
 - Interpreter.hs
 - v1.0
 -----------------------------------}

module Interpreter where

-- Language Representation
import Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec
import Parser

fixMe = error "fix me!"

liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBool _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

{-----------------------------------
 - eval: The Evaluator
 -----------------------------------}
eval :: Exp -> Env -> Val
eval (IntExp x) _ = IntVal x

eval (BoolExp x) _ = BoolVal x

eval (VarExp s) env = 
        case (H.lookup s env) of
           Just v -> v
           Nothing -> ExnVal "No match in env"

eval (IntOpExp op a b) env = 
        case H.lookup op intOps of
             Just v -> let x = eval a env; y = eval b env in liftIntOp v x y
             Nothing -> ExnVal "No matching operator"

eval (BoolOpExp op a b) env =
        case H.lookup op boolOps of
            Just v -> let x = eval a env; y = eval b env in liftBoolOp v x y
            Nothing -> ExnVal "No matching operator"

eval (CompOpExp op a b) env =
        case H.lookup op compOps of
            Just v -> let x = eval a env; y = eval b env in liftCompOp v x y
            Nothing -> ExnVal "No matching operator"

eval (LetExp [] e2) env = eval e2 env        
eval (LetExp ((s,e1):xs) e2) env =
                let v1 = eval e1 env in eval (LetExp xs e2) (insert s v1 env)

eval (FunExp xx e1) env = CloVal xx e1 env      

eval (AppExp e1 xx) env =
    case (eval e1 env) of  -- e1 can be FunExp, VarExp, etc! Create pattern match exception
        CloVal v e3 cenv ->
            let arg = Prelude.map (flip eval env) xx  
            in case (compare (length v) (length xx)) of 
                GT -> CloVal (drop (length xx) v) e3 (union (fromList(zip (take (length xx) v) arg)) cenv)
                EQ -> eval e3 (union (fromList(zip v arg)) cenv)
        _ -> ExnVal "Expression not a closure."

eval (IfExp e1 e2 e3) env =
        case (eval e1 env) of 
            BoolVal True -> eval e2 env
            BoolVal False -> eval e3 env
            _ -> ExnVal "Guard is not a boolean."
{-----------------------------------
 - exec
 -----------------------------------}
exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
   where val = show $ eval e env
   
exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = (s1++s2,penv',env')
   where (s1,penv'',env'') = exec x penv env
         (s2,penv',env') = exec (SeqStmt xs) penv'' env''

exec (IfStmt a s1 s2) penv env =            -- Notice: exp a instead of BoolOpExp! Could be other expressions!!
                    case (eval a env) of 
                         BoolVal True -> exec s1 penv env
                         BoolVal False -> exec s2 penv env
                         _ -> ("Guard is not a boolean.", penv, env)
                         
exec (SetStmt s e) penv env = ("",penv,insert s (eval e env) env)

exec (ProcedureStmt f ps body) penv env = ("",insert f (ProcedureStmt f ps body) penv,env)

exec (CallStmt name arg) penv env =
     case (H.lookup name penv) of
          (Just (ProcedureStmt f ps body)) -> 
                let val = Prelude.map (flip eval env) arg
                in exec body penv (union (fromList(zip ps val)) env)
          Nothing -> ("Procedure f undefined",penv,env)
        
        
        
        
{-----------------------------------
 - repl
 -----------------------------------}
repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"
