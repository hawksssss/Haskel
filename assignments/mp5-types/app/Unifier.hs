module Unifier where
import qualified Data.HashMap.Strict as H
import Lib

fixMe = error "fix me!"

{---------------------------------------------
 - Given Demo Environments
 -   You might not want to modify this. ;)
 ---------------------------------------------}
phi :: SubstEnv
phi = (H.insert 5 (FnTy BoolTy (TyVar 2)) H.empty)

{---------------------------------------------
 - Problem 1: substFun
 ---------------------------------------------}
substFun :: SubstEnv -> TyCon -> TyCon
substFun = fixMe

{---------------------------------------------
 - Problem 2: monoTyLiftSubst
 ---------------------------------------------}
monoTyLiftSubst :: SubstEnv -> TyCon -> TyCon
monoTyLiftSubst = fixMe

{---------------------------------------------
 - Problem 3: occurs
 ---------------------------------------------}
occurs :: TyCon -> TyCon -> Bool
occurs = fixMe

{---------------------------------------------
 - Problem 4: unify
 ---------------------------------------------}
unify :: EqnSet -> Maybe SubstEnv
unify = fixMe
