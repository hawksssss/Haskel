module Main where

import Lib

import Text.Regex.TDFA

main :: IO ()
main = someFunc

isInt :: String -> Bool
isInt i = i =~ "[0-9]+"

isSymbol :: String -> String -> Bool
isSymbol s v = s == v

parseSymbol s (x:xs) =
  if s == x
     then (s,xs)
     else error $ "Parse error, expected " ++ s ++ " but got " ++ x ++ "."

-- Grammar
--
-- E -> + E E
--    | int
--    | var
--    | let var = E in E end

data Exp = PlusExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LetExp String Exp Exp
    deriving Show

parse xx = parseE (words xx)

parseE ("+":xs) =
  let (e1,r1) = parseE xs
      (e2,r2) = parseE r1
   in (PlusExp e1 e2, r2)

parseE (x:xs) | isInt x =
                (IntExp (read x), xs)

parseE ("let":x:xs) = 
  let v = x
      (_,r1) = parseSymbol "=" xs
      (e1,r2) = parseE r1
      (_,r3) = parseSymbol "in" r2
      (e2,r4) = parseE r3
      (_,r5) = parseSymbol "end" r4
  in (LetExp v e1 e2, r5)

parseE (x:xs) = (VarExp x, xs)

