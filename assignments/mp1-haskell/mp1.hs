module Mp1 where

data Cons a = Cons a (Cons a)
            | Nil
  deriving (Show,Eq)

data Exp = IntExp Int
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show,Eq)

{-
 - You'll want to go through and put in the type signatures first, if you want
 - to check your work incrementally with quickCheck. (quickCheck is a way to
 - create random tests to validate your work.)
 -
 - To use our quickChecks, you'll want to load up mp1check by going:
 -   :l mp1check
 -
 - then for each property in mp1check, you'll want to test with:
 -   > quickCheck PROP_NAME
 -
 - For example, to test mytake, you would run:
 -   > quickCheck prop_mytake
 -}

-- mytake :: your type signature here
mytake = undefined

-- mydrop :: your type signature here
mydrop = undefined

-- rev :: your type signature here
rev = undefined

-- app :: your type signature here
app = undefined

-- add :: your type signature here
add = undefined

-- union :: your type signature here
union = undefined

-- intersect :: your type signature here
intersect = undefined

-- powerset :: your type signature here
powerset = undefined

-- inclist :: your type signature here
inclist = undefined

-- sumlist :: your type signature here
sumlist = undefined

-- myzip :: your type signature here
myzip = undefined

-- addpairs :: your type signature here
addpairs = undefined

-- ones :: your type signature here
ones = undefined

-- nats :: your type signature here
nats = undefined

-- fib :: your type signature here
fib = undefined

-- list2cons :: your type signature here
list2cons = undefined

-- cons2list :: your type signature here
cons2list = undefined

-- eval :: your type signature here
eval = undefined

-- inclist' :: your type signature here
inclist' = undefined

-- sumlist' :: your type signature here
sumlist' = undefined

-- list2cons' :: your type signature here
list2cons' = undefined
