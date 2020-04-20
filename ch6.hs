import Prelude hiding ((^), and, concat, replicate, (!!), elem)

-- 1.  How does the recursive version of the factorial function behave if applied to a negative argument,such as (-1) ? Modify the definition to prohibit negative arguments by adding a guard to the
-- 	recursive case.

factorial :: Integer -> Integer
factorial x 
    | x == 0 = 1
    | x > 0  = x * factorial (x-1)
    | x < 0  = 0 -- Random case


-- 2.  Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative
--     integers from a given value down to zero. For example, sumdown 3 should return the result
--     3+2+1+0 = 6 .

sumdown :: Int -> Int
sumdown x
    | x == 0 = 0
    | x > 0  = x + sumdown (x-1)
    | x < 0  = 0


-- 3.  Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion
--     as the multiplication operator * , and show how the expression 2 ^ 3 is evaluated using your
--     definition.

(^) :: Num a => a -> Int -> a
m ^ 1 = m
m ^ n = m * (m ^ (n-1))

{-  Evaluation steps

> 2 ^ 3

2 * (2 ^ 2)
2 * (2 * (2 ^ 1))
2 * (2 * (2))
8

-}


-- 4.  Define a recursive function euclid :: Int -> Int -> Int that implements Euclid’s algorithm
--     for calculating the greatest common divisor of two non-negative integers: if the two numbers are
--     equal, this number is the result; otherwise, the smaller number is subtracted from the larger, and the
--     same process is then repeated. For example:
--     > euclid 6 27
--     3


euclid :: Int -> Int -> Int
euclid a b
    | a == b = a
    | a < b  = euclid a (b-a)
    | a > b  = euclid (a-b) b



-- 6.  Without looking at the definitions from the standard prelude, define the following library functions
--     on lists using recursion.

    
-- a. Decide if all logical values in a list are True :

and :: [Bool] -> Bool
and [] = True
and (x:xs) 
    | x == False = False
    | otherwise  = and xs


-- b.  Concatenate a list of lists:

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs


-- c.  Produce a list with n identical elements:

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : (replicate (n-1) a)


-- d.  Select the n th element of a list:

(!!) :: [a] -> Int -> a
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)


-- e.  Decide if a value is an element of a list:

elem :: Eq a => a -> [a] -> Bool
elem e [] = False
elem e (x:xs)
    | e == x = True
    | e /= x = elem e xs


-- 7.  Define a recursive function merge :: Ord a => [a] -> [a] -> [a]
--     that merges two sorted lists to give a single sorted list. For example:
--     > merge [2,5,6] [1,3,4]
--     [1,2,3,4,5,6]

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | x > y  = y : merge (x:xs) ys

-- 8.  Using merge , define a function msort :: Ord a => [a] -> [a] that implements merge sort, in
--     which the empty list and singleton lists are already sorted, and any other list is sorted by merging
--     together the two lists that result from sorting the two halves of the list separately.
--     Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose
--     lengths differ by at most one.

halve :: [a] -> ([a],[a])
halve xs = (x,y)
    where x = take n xs
          y = drop n xs
          n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort xs 
    | length xs < 2 = xs
    | otherwise     = merge (msort fhalve) (msort shalve)
    where fhalve = fst (halve xs)
          shalve = snd (halve xs)


-- 9.  Using the five-step process, construct the library functions that:


-- a. calculate the sum of a list of numbers;

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs


-- b. take a given number of elements from the start of a list;

taken :: Int -> [a] -> [a]
taken 0 _  = []
taken _ [] = []
taken n (x:xs) = x : taken (n-1) xs


-- c. select the last element of a non-empty list.

lastele :: [a] -> a
lastele [x] = x
lastele (_:xs) = lastele xs