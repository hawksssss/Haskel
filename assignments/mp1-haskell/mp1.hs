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
 -aa
 - To use our quickChecks, you'll want to load up mp1check by going:
 -   :l mp1check
 -
 - then for each property in mp1check, you'll want to test with:
 -   > quickCheck PROP_NAME
 -
 - For example, to test mytake, you would run:
 -   > quickCheck prop_mytake 
 -}

mytake :: Int -> [Int] -> [Int]
mytake n xx 
        | n<=0 = []
mytake n [] = []
mytake n (x:xs) = x:(mytake (n-1) xs)

mydrop :: Int -> [Int] -> [Int]
mydrop n xx 
       | n<=0 = xx
mydrop n [] = []
mydrop n (x:xs) = mydrop (n-1) xs

rev :: [Int] -> [Int]
rev xx = aux xx [] where
    aux [] aa = aa
    aux (x:xs) aa = aux xs (x:aa)

app :: [Int] -> [Int] -> [Int]
app xx [] = xx
app xx (y:ys) = app (xx ++ [y]) ys 

add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x xx = [y| y<- xx, y<x] ++ [x] ++ [y| y<- xx, y>x]

union :: Ord a => [a] -> [a] -> [a]
union [] yy = yy
union (x:xs) yy = union xs (add x yy)

intersect :: Ord a => [a] -> [a] -> [a]
intersect [] yy = []
intersect (x:xs) yy = [y | y<-yy, y==x] ++ intersect xs yy

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [[]] ++ [x:xx | xx<- (powerset xs)] ++ [xx | xx<- (powerset xs),xx/=[]]

inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x:xs) = x+1:inclist xs

sumlist :: (Num t) => [t] -> t
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

myzip :: [t] -> [t1] -> [(t, t1)]
myzip [] xx = []
myzip xx [] = []
myzip (x:xs) (y:ys) = (x,y):myzip xs ys

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] xx = []
addpairs xx [] = []
addpairs (x:xs) (y:ys) = (x+y):addpairs xs ys

ones :: [Integer]
ones = 1:ones

nats :: [Integer]
nats = 1 : (inclist nats)

fib :: [Integer]
fib = 1 : (addpairs fib (0:fib))

list2cons :: [a] -> Cons a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

cons2list :: Cons a -> [a]
cons2list Nil = []
cons2list (Cons x xs) =x : cons2list xs

eval :: Exp -> Int
eval (IntExp a) = a
eval (PlusExp [a, b]) = (+) (eval a) (eval b)
eval (MultExp [a, b]) = (*) (eval a) (eval b)

inclist' :: (Num a) => [a] -> [a]
inclist' xx = map (+1) xx

sumlist' :: (Num a) => [a] -> a
sumlist' xx = foldr (+) 0 xx

list2cons' :: [a] -> Cons a
list2cons' xx = foldr Cons Nil xx
