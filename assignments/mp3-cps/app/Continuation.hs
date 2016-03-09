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

factk 0 k = k 1
factk n k = factk (n-1) (\ v -> k (n * v))

{--------------------------------------
 - Problem 2: evenoddk
 --------------------------------------}
evenoddk :: Integral r => [r] -> (r -> t) -> (r -> t) -> t
evenoddk [x] k1 k2
         | even x = k1 x
         | odd x = k2 x
evenoddk (x:xs) k1 k2
            | even x = evenoddk xs (\v -> k1 (v + x)) k2
            | odd x = evenoddk xs k1 (\v -> k2 (v + x))


{--------------------------------------
 - Problem 3: isSimple
 --------------------------------------}
isSimple :: Exp -> Bool

isSimple (IntExp a) = True

isSimple (VarExp s) = True

isSimple (AppExp e1 e2) = False

isSimple (OpExp s e1 e2) = isSimple e1 && (isSimple e2)

isSimple (IfExp e1 e2 e3) = isSimple e1 && (isSimple e2) && (isSimple e3)


{--------------------------------------
 - Problem 4: cpsDecl, cpsExp
 --------------------------------------}
cpsDecl :: Stmt -> Stmt

cpsDecl (Decl s1 xx e) = Decl s1 (xx++["k"]) e1
          where (e1,_) = cpsExp e (VarExp "k") 1

cpsExp :: Exp -> Exp -> Integer -> (Exp,Integer)
cpsExp (IntExp i) k j = (AppExp k (IntExp i), j)
cpsExp (VarExp v) k j = (AppExp k (VarExp v), j)
cpsExp (AppExp e1 e2) k j
              | isSimple e2 = (AppExp (AppExp e1 e2) k, j)
              | otherwise = cpsExp e2 (LamExp m (AppExp (AppExp e1 (VarExp m)) k)) (j + 1)
                  where (m,_) = gensym j

cpsExp (IfExp e1 e2 e3) k j
                 | isSimple e1 = (IfExp e1 e4 e5, j)
                 | otherwise = cpsExp e1 (LamExp m (IfExp (VarExp m) e4 e5)) (j + 1)
                     where (e4,_) = cpsExp e2 k j
                           (e5,_) = cpsExp e3 k j
                           (m,_) = gensym j

cpsExp (OpExp s e1 e2) k j
              | isSimple e1 && (isSimple e2) = (AppExp k (OpExp s e1 e2), j)         
              | not (isSimple e1) && (isSimple e2) = cpsExp e1 (LamExp v (AppExp k (OpExp s (VarExp v) e2))) (j + 1)
              | isSimple e1 && (not (isSimple e2)) = cpsExp e2 (LamExp v (AppExp k (OpExp s e1 (VarExp v)))) (j + 1)
              | otherwise = cpsExp e1 (LamExp v e3) l
                 where (v,_) = gensym j
                       (e3,l) = cpsExp e2 (LamExp v1 (AppExp k (OpExp s (VarExp v) (VarExp v1)))) (j + 2)
                       (v1,_) = gensym (j + 1)
                       
{--------------------------------------
 - Helper Functions
 --------------------------------------}

gensym :: Integer -> (String,Integer)
gensym i = ("v" ++ show i, i + 1)
