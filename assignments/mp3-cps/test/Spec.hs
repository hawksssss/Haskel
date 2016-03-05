import Autograder
import Control.Monad
import Data.List
import Lib
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import qualified Continuation as C

factTests' = [
              10
            ]

evenoddTests' = [
                 [1]
               , [1,2]
               ]

isSimpleTests = [
                ]

main = do {
          ; putStrLn ""
          ; testProp (prop_factk, "factk", factTests)
          ; testProp (prop_evenoddk, "evenoddk", evenoddTests)
          ; testProp (prop_isSimple, "isSimple", isSimpleTests)
          }

{- DO NOT MODIFY BELOW THIS LINE! -}

instance Arbitrary Stmt where
  arbitrary = sized stmt'

stmt' n = (liftM3 Decl fname params expr)
           where
             paramList = take (mod n 26) (map (\x -> [x]) ['a' .. 'z'])
             fname = listOf1 (elements ['a' .. 'z'])
             expr = exp' paramList
             params = (sublistOf paramList)

instance Arbitrary Exp where
  arbitrary = exp' alphastring
    where
      alphastring = (map (\x -> [x]) ['a' .. 'z'])

exp' alphastring = frequency [
         (10, liftM VarExp (elements alphastring)),
         (10, liftM IntExp arbitrary),
         (1, liftM2 AppExp sub sub),
         (5, liftM3 IfExp sub sub sub),
         (5, liftM3 OpExp op sub sub),
         (1, liftM2 LamExp (elements alphastring) sub)
       ]
 where
   sub = exp' alphastring
   op = elements ["*", "/", "+", "-", "<", ">", "==", "<=", "=>", "/="]

{----------------------------------
 - Tests for factk
 ----------------------------------}

factTests = map Positive factTests'

factorial 0 = 1
factorial n = factorial (n-1) * n

prop_factk :: Positive Integer -> Bool
prop_factk (Positive x) = (C.factk x (\y -> y)) == factorial x

{----------------------------------
 - Tests for evenodd
 ----------------------------------}

evenoddTests = map (NonEmpty) evenoddTests'

prop_evenoddk :: NonEmptyList Int -> Bool
prop_evenoddk (NonEmpty xs) = (C.evenoddk xs id id) == (if lastElem == 0 then resultEven else resultOdd)
  where lastElem = mod (xs !! (length xs - 1)) 2
        resultEven = sum (filter even xs)
        resultOdd = sum (filter odd xs)

{----------------------------------
 - Tests for isSimple
 ----------------------------------}

prop_isSimple :: Exp -> Bool
prop_isSimple x = (not (isInfixOf "LamExp" x') && (C.isSimple x == not (isInfixOf "AppExp" x'))) || ((isInfixOf "LamExp" x'))
  where
    x' = show x
