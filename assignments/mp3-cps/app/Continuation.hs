module Continuation where
import Lib

fixMe = error "fix me!"

repl () =
  do putStr "CPS> "
     input <- getLine
     case parseDecl input of
        Right x -> let result = cpsDecl x
                    in do putStrLn "Pretty Result: "
                          putStrLn $ toStr result
                          putStrLn "Details: "
                          putStrLn $ show result
                          putStrLn ""
                          repl ()
        Left x -> do putStrLn $ show x
                     repl ()

{--------------------------------------
 - Problem 1: factk 
 --------------------------------------}
factk :: Integer -> (Integer -> a) -> a
factk = fixMe

{--------------------------------------
 - Problem 2: evenoddk
 --------------------------------------}
evenoddk :: Integral r => [r] -> (r -> t) -> (r -> t) -> t
evenoddk = fixMe

{--------------------------------------
 - Problem 3: isSimple
 --------------------------------------}
isSimple :: Exp -> Bool
isSimple = fixMe

{--------------------------------------
 - Problem 4: cpsDecl, cpsExp
 --------------------------------------}
cpsDecl :: Stmt -> Stmt
cpsDecl = fixMe

cpsExp :: Exp -> Exp -> Integer -> (Exp,Integer)
cpsExp = fixMe

{--------------------------------------
 - Helper Functions
 --------------------------------------}

gensym :: Integer -> (String,Integer)
gensym i = ("v" ++ show i, i + 1)
