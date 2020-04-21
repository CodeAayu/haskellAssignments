import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry, iterate)
import Data.Char

-- 1.  Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-
--     order functions map and filter .

ans f p xs = map f (filter p xs)


-- 2.  Without looking at the definitions from the standard prelude, define the following higher-order
--     library functions on lists.


-- a.  Decide if all elements of a list satisfy a predicate:

all :: (a -> Bool) -> [a] -> Bool
all q = and . map q

-- b.  Decide if any element of a list satisfies a predicate:

any :: (a -> Bool) -> [a] -> Bool
any q = or . map q

-- c.  Select elements from a list while they satisfy a predicate:

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs) | f x       = x : takeWhile f xs
                   | otherwise = []

-- d.  Remove elements from a list while they satisfy a predicate:

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs) | f x       = dropWhile f xs
                   | otherwise = x:xs 


-- 3.  Redefine the functions map f and filter p using foldr.

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x:xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x xs -> if f x then x:xs else xs) [] 


-- 4.  Using foldl , define a function dec2int :: [Int] -> Int that converts a decimal number into
--     an integer. For example:
--     > dec2int [2,3,4,5]
--     2345

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x*10 + y) 0


-- 5.  Without looking at the definitions from the standard prelude, define the higher-order library
--     function curry that converts a function on pairs into a curried function, and, conversely, the
--     function uncurry that converts a curried function with two arguments into a function on pairs.
--     Hint: first write down the types of the two functions.

curry :: ((x,y)->z) -> (x -> y -> z)
curry p = \x y -> p (x,y)

uncurry :: (x -> y -> z) -> ((x,y)->z)
uncurry p = \(x,y) -> p x y


-- 6.  A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list
--     can be defined as follows:
     
--     unfold p h t x | p x       = []
--                    | otherwise = h x : unfold p h t (t x)

--     That is, the function unfold p h t produces the empty list if the predicate p is true of the
--     argument value, and otherwise produces a non-empty list by applying the function h to this value to
--     give the head, and the function t to generate another argument that is recursively processed in the
--     same way to produce the tail of the list. For example, the function int2bin can be rewritten more
--     compactly using unfold as follows:

--     int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
    
--     Redefine the functions chop8 , map f and iterate f using unfold .

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map' :: (a->b) -> [a] -> [b]
map' f = unfold (null) (f . head) (tail)

iterate :: (a->a) -> a -> [a]
iterate f = unfold (\a -> False) (\a -> a) (\a -> f a)


-- 7.  Modify the binary string transmitter example to detect simple transmission errors using the concept
--     of parity bits. That is, each eight-bit binary number produced during encoding is extended with a
--     parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn,
--     each resulting nine-bit binary number consumed during decoding is checked to ensure that its parity
--     bit is correct, with the parity bit being discarded if this is the case, and a parity error being
--     reported otherwise.
--     Hint: the library function error :: String -> a displays the given string as an error message
--     and terminates the program; the polymorphic result type ensures that error can be used in any
--     context.

bin2int :: [Bit] -> Int
bin2int xs = sum [w*b | (w,b) <- zip (iterate (*2) 1) xs]

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parityBit :: [Bit] -> Bit
parityBit xs | even (sum xs) = 0
             | otherwise     = 1

addParityBit :: [Bit] -> [Bit]
addParityBit xs = (parityBit xs) : xs

encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

take9 :: [Bit] -> [[Bit]]
take9 [] = []
take9 xs = take 9 xs : take9 (drop 9 xs)

removeParity :: [Bit] -> [Bit]
removeParity (x:xs) | x == parityBit xs = xs
                    | otherwise         = error "Error in transmission, Parity bit doesn't match"

decode :: [Bit] -> String
decode = map (chr . bin2int . removeParity) . take9


-- 9.  Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies
--     its two argument functions to successive elements in a list, in turn about order. For example:
--     > altMap (+10) (+100) [0,1,2,3,4]
--     [10,101,12,103,14]

altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f g []       = []
altMap f g [x]      = [f x]
altMap f g [x,y]    = [f x, g y]
altMap f g (x:y:xs) = [f x, g y] ++ altMap f g xs


-- 10. Using altMap , define a function luhn :: [Int] -> Bool that implements the Luhn algorithm
--     from the exercises in chapter 4 for bank card numbers of any length. Test your new function using
--     your own bank card.

luhnTest :: Int -> Int
luhnTest x = if x > 9
                 then x - 9
                 else x

luhn :: [Int] -> Bool
luhn a = ( sum . map luhnTest . altMap (*2) (*1) ) a `mod` 10 == 0