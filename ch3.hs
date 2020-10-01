
-- 1. What are the types of the following values?

{-
['a','b','c'] :: [Char]

('a','b','c') :: (Char,Char,Char)

[(False,'O'),(True,'1')] :: [(Bool,Char)]

([False,True],['0','1']) :: ([Bool],[Char])

[tail, init, reverse] :: [[a]->[a]]
-}

-- 2. Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.

bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1..5],[6..10],[11..15]]

add :: Int -> Int -> Int -> Int
add a b c = a+b+c

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a->b) -> a -> b
apply function something = function something

-- 3. What are the types of the following functions?

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
