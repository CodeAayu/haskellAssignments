-- 1. Using library functions, define a function halve :: [a] -> ([a],[a])
--    that splits an even-lengthed list into two halves.

halve :: [a] -> ([a],[a])
halve x = (take halfLength x, drop halfLength x)
    where halfLength = length x `div` 2

-- 2. Define a function third :: [a] -> a that returns the third element 
--    in a list that contains at least this many elements using:
--     a. head and tail
--     b. list indexing !! 
--     c. pattern matching.

thirda :: [a] -> a
thirda x = head(tail(tail x))

thirdb :: [a] -> a
thirdb x = x !! 2

thirdc :: [a] -> a
thirdc (x:y:z:_) = z

-- 3. Consider a function safetail :: [a] -> [a] that behaves in the same 
--     way as tail except that it maps the empty list to itself rather than 
--     producing an error. Using tail and the function null :: [a] -> Bool that 
--     decides if a list is empty or not, define safetail using:
--     a. a conditional expression;
--     b. guarded equations;
--     c. pattern matching.

safetaila :: [a] -> [a]
safetaila x = if n>0
                then tail x
                else []
    where n = length x

safetailb :: [a] -> [a]
safetailb x | null x    = []
            | otherwise = tail x

safetailc :: [a] -> [a]
safetailc [] = []
safetailc (x:xs) = xs

-- 4.  In a similar way to && in section 4.4, show how the disjunction operator
--     || can be defined in four different ways using pattern matching.

(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False

(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

(||) :: Bool -> Bool -> Bool
True || _    = True
_    || True = True
_    || _    = False

(||) :: Bool -> Bool -> Bool
False || a = a
_     || _ = True


-- 5.  Without using any other library functions or operators, show how 
--     the meaning of the following pattern matching definition for logical
--     conjunction && can be formalized using conditional expressions:
    
--     True && True = True
--        _ && _    = False

--     Hint: use two nested conditional expressions.

(&&) :: Bool -> Bool -> Bool
a && b  = if a == True
            then if b == True 
                   then True 
                   else False
            else False


-- 6.  Do the same for the following alternative definition, and note the 
--     difference in the number of conditional expressions that are required:
    
--     True && b = b
--     False && _ = False

(&&) :: Bool -> Bool -> Bool
a && b  = if a == True
            then b
            else False


-- 7.  Show how the meaning of the following curried function definition can 
--     be formalized in terms of lambda expressions:
    
--     mult :: Int -> Int -> Int -> Int
--     mult x y z = x*y*z

mult :: Int -> Int -> Int -> Int
mult = \a -> (\b -> (\c -> a * b * c))

-- 8.  The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a
--     digit, and proceeds as follows:
--     -> consider each digit as a separate number;
--     -> moving left, double every other number from the second last;
--     -> subtract 9 from each number that is now greater than 9;
--     -> add all the resulting numbers together;
--     -> if the total is divisible by 10, the card number is valid.
    
--     Define a function luhnDouble :: Int -> Int
--     that doubles a digit and subtracts 9 if the result isgreater than 9 .

--     Using luhnDouble and the integer remainder function mod , define a
--     function luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank
--     card number is valid.

luhnDouble :: Int -> Int
luhnDouble x = if x*2 > 9
                 then 2 * x - 9
                 else 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (x+y+b+d) `mod` 10 == 0
    where
        x = luhnDouble a
        y = luhnDouble c