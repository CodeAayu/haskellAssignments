-- 1.  In a similar manner to the function add , define a recursive multiplication function 
-- 	mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
-- 	Hint: make use of add in your definition.

data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addNat :: Nat -> Nat -> Nat
addNat Zero n     = n
addNat (Succ m) n = Succ (addNat m n)

mult :: Nat -> Nat -> Nat
mult Zero n        = Zero
mult (Succ Zero) n = n
mult (Succ m) n    = addNat n (mult m n)


-- 2.  Although not included in appendix B, the standard prelude defines
--     data Ordering = LT | EQ | GT together with a function
--     compare :: Ord a => a -> a -> Ordering
--     that decides if one value in an ordered type is less than ( LT ), equal to ( EQ ), or greater than ( GT )
--     another value. Using this function, redefine the function occurs :: Ord a => a -> Tree a ->
--     Bool for search trees. Why is this new definition more efficient than the original version?

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b)     = a == b
occurs a (Node l b r) = case compare a b of
                            EQ -> True
                            LT -> occurs a l 
                            GT -> occurs a r


-- 3.  Consider the following type of binary trees:
--     data Tree a = Leaf a | Node (Tree a) (Tree a)
--     Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every
--     node differs by at most one, with leaves themselves being trivially balanced. Define a function
--     balanced :: Tree a -> Bool that decides if a binary tree is balanced or not.
--     Hint: first define a function that returns the number of leaves in a tree.

data BalTree a = BalLeaf a | BalNode (BalTree a) (BalTree a) deriving (Show)

leaveCount :: BalTree a -> Int
leaveCount (BalLeaf _)   = 1
leaveCount (BalNode l r) = leaveCount l + leaveCount r

balanced :: BalTree a -> Bool
balanced (BalLeaf _)   = True
balanced (BalNode l r) = balanced l && balanced r && abs(leaveCount l - leaveCount r) < 2


-- 4.  Define a function balance :: [a] -> Tree a that converts a non-empty list into a balanced tree.
--     Hint: first define a function that splits a list into two halves whose length differs by at most one.

splitHalf :: [a] -> ([a],[a])
splitHalf xs = (take n xs , drop n xs)
                  where n = length xs `div` 2

balance :: [a] -> BalTree a
balance [x] = BalLeaf x
balance xs  = BalNode (balance (fst hlf)) (balance (snd hlf))
                  where hlf = splitHalf xs


-- 5.  Given the type declaration
--     data Expr = Val Int | Add Expr Expr
--     define a higher-order function
--     folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
--     such that folde f g replaces each Val constructor in an expression by the function f , and each
--     Add constructor by the function g.

data Expre = Vale Int | Adde Expre Expre

folde :: (Int -> a) -> (a -> a -> a) -> Expre -> a
folde f _ (Vale x)   = f x
folde f g (Adde x y) = g (folde f g x) (folde f g y)


-- 6.  Using folde , define a function eval :: Expr -> Int that evaluates an expression to an integer
--     value, and a function size :: Expr -> Int that calculates the number of values in an expression.

myadd :: Num a => a -> a -> a
myadd x y = x + y

myid :: a -> a
myid x = x

evale :: Expre -> Int
evale x = folde myid myadd x


-- 7.  Complete the following instance declarations:
--     instance Eq a => Eq (Maybe a) where
--     ...
--     instance Eq a => Eq [a] where
--     ...

data MyMaybe a = MyJust a | MyNothing

instance Eq a => Eq (MyMaybe a) where
    MyJust a  == MyJust b  = a==b
    MyNothing == MyNothing = True
    _         == _         = False

data MyList a = MyList [a] 

instance Eq a => Eq (MyList a) where
    MyList []     == MyList []     = True
    MyList (a:xs) == MyList (b:ys) = a==b && MyList xs == MyList ys
    _             == _             = False


-- 8.  Extend the tautology checker to support the use of logical disjunction and equivalence in
--     propositions.

type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']
type Subst = Assoc Char Bool

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Equiv Prop Prop

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q


-- 9. Extend the abstract machine to support the use of multiplication.

type Cont = [Op]
data Op   = EVALA Expr | EVALM Expr | ADD Int | MUL Int
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

eval9 :: Expr -> Cont -> Int
eval9 (Val n)   c = exec c n
eval9 (Add x y) c = eval9 x (EVALA y : c)
eval9 (Mul x y) c = eval9 x (EVALM y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA y : c) n = eval9 y (ADD n : c)
exec (EVALM y : c) n = eval9 y (MUL n : c)
exec (ADD y : c)   n = exec c (y+n)
exec (MUL y : c)   n = exec c (y*n)

value :: Expr -> Int
value e = eval9 e []