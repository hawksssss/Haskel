module Main where

import Lib

import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.List (intersperse)

data Entity = Var String
            | Object String [Entity]
  deriving (Eq)

instance Show Entity where
  show (Var s) = s
  show (Object s []) = s
  show (Object f xx) = concat $ f : "(" : intersperse "," (map show xx) ++ [")"]

type Env = H.HashMap String Entity

initial :: Env
initial = H.empty

add :: String -> Entity -> Env -> Env
add x y b = H.insert x y b

contains :: String -> Env -> Bool
contains x b = H.member x b

unifyVar :: Entity -> Env -> Entity
unifyVar x@(Var t) bindings
   | contains t bindings = fromJust $ H.lookup t bindings
   | otherwise = x
unifyVar x _ = x

unify :: Entity -> Entity -> Env -> Env
unify x y bindings = aux (unifyVar x bindings) (unifyVar y bindings) bindings
  where aux (Var s) x bindings = add s x bindings
        aux x (Var s) bindings = -- ???
        aux (Object f ff) (Object g gg) bindings = -- ??



        aux _ _ _ = H.empty
