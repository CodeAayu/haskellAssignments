-- 1.  Redefine the combinatorial function choices using a list comprehension rather than using
--     composition, concat and map.

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [ x | y <- subs xs , x <- perms y ]


-- 2.  Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool that decides if one list
--     is chosen from another, without using the combinatorial functions perms and subs . Hint: start by
--     defining a function that removes the first occurrence of a value from a list.

removeFstOcc :: Eq a => a -> [a] -> [a]
removeFstOcc x []              = [] 
removeFstOcc x (y:ys) | x == y = ys
                      | x /= y = y : removeFstOcc x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _ []      = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFstOcc x ys)


-- 4.  Using the functions choices , exprs , and eval , verify that there are 33,665,406 possible
--     expressions over the numbers 1, 3, 7, 10, 25, 50, and that only 4,672,540 of these expressions
--     evaluate successfully.

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
valid Exp x y = y >= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                           where
                               brak (Val n) = show n
                               brak e       = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [ apply o x y | x <- eval l , y <- eval r, valid o x y]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]


totalExpr :: [Int] -> Int
totalExpr = length . concat . map exprs . choices

succExpr :: [Int] -> Int
succExpr = length . filter ( /= []) . map eval . concat . map exprs . choices


-- 5.  Similarly, verify that the number of expressions that evaluate successfully increases to 10,839,369
--     if the numeric domain is generalised to arbitrary integers. Hint: modify the definition of valid .


valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub x y = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && x `mod` y == 0
valid' Exp x y = y >= 0

eval' :: Expr -> [Int]
eval' (Val n)     = [n | n > 0]
eval' (App o l r) = [ apply o x y | x <- eval' l , y <- eval' r, valid' o x y]

succExpr' :: [Int] -> Int
succExpr' = length . filter ( /= []) . map eval' . concat . map exprs . choices


-- 6.  Modify the final program to:

-- a.  allow the use of exponentiation in expressions.

{-
Added exponent to lines 40, 47, 54, 61, 82 and 112.
-}

-- b. produce the nearest solutions if no exact solution is possible;
-- c. order the solutions using a suitable measure of simplicity.
{-
Couldn't understand the parts, will update once done.
-}
