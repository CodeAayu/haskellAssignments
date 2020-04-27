-- 1.  Define an instance of the Functor class for the following type of binary trees that have data in
--     their nodes:
--     data Tree a = Leaf | Node (Tree a) a (Tree a)
--             deriving Show

data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

instance Functor Tree where
    fmap f Leaf         = Leaf
    fmap f (Node a b c) = Node (fmap f a) (f b) (fmap f c)


-- 2.  Complete the following instance declaration to make the partially-applied function type (a ->)
--     into a functor:
--     instance Functor ((->) a) where
--     ...
--     Hint: first write down the type of fmap , and then think if you already know a library function that
--     has this type.

instance Functor ((->) a) where
    fmap = (.)


-- 3.  Define an instance of the Applicative class for the type (a ->) . If you are familiar with
--     combinatory logic, you might recognise pure and <*> for this type as being the well-known K and
--     S combinators.

instance Applicative ((->) a) where
    pure = const
    f <*> g = \x -> f x (g x)


-- 4.  There may be more than one way to make a parameterised type into an applicative functor. For
--     example, the library Control.Applicative provides an alternative ‘zippy’ instance for lists, in
--     which the function pure makes an infinite list of copies of its argument, and the operator <*>
--     applies each argument function to the corresponding argument value at the same position. Complete
--     the following declarations that implement this idea:

--     newtype ZipList a = Z [a] deriving Show
--     instance Functor ZipList where
    
--     -- fmap :: (a -> b) -> ZipList a -> ZipList b
--     fmap g (Z xs) = ...
    
--     instance Applicative ZipList where
--     -- pure :: a -> ZipList a
--     pure x = ...
--     -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
--     (Z gs) <$> (Z xs) = ...

--     The ZipList wrapper around the list type is required because each type can only have at most one
--     instance declaration for a given class.

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (g xs)
    
instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]


-- 6.  Define an instance of the Monad class for the type (a ->).

instance Monad ((->) a) where
    g >>= h = \x -> h (g x) x


-- 7.  Given the following type of expressions
--         data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
--         deriving Show
--     that contain variables of some type a , show how to make this type into instances of the Functor ,
--     Applicative and Monad classes. With the aid of an example, explain what the >>= operator for
--     this type does.

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
        deriving Show

instance Functor Expr where
    fmap _ (Val x)   = Val x
    fmap g (Var x)   = Var (g x)
    fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
    pure = Var
    Var g <*> h = fmap g h

instance Monad Expr where
    Var x >>= g   = g x
    Val x >>= g   = Val x
    Add x y >>= g = Add (x >>= g) (y >>= g)


-- 8.  Rather than making a parameterised type into instances of the Functor , Applicative and Monad
--     classes in this order, in practice it is sometimes simpler to define the functor and applicative
--     instances in terms of the monad instance, relying on the fact that the order in which declarations are
--     made is not important in Haskell. Complete the missing parts in the following declarations for the
--     ST type using the do notation.

--     instance Functor ST where
--     -- fmap :: (a -> b) -> ST a -> ST bfmap g st = do ...
    
--     instance Applicative ST where
--     -- pure :: a -> ST a
--     pure x = S (\s -> (x,s))
--     -- (<*>) :: ST (a -> b) -> ST a -> ST b
--     stf <*> stx = do ...

--     instance Monad ST where
--     -- (>>=) :: ST a -> (a -> ST b) -> ST b
--     st >>= f = S (\s ->
--     let (x,s’) = app st s in app (f x) s’)

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do x <- st
                   S (\s -> ((g x), s))
    
instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf
                     x <- stx
                     S (\s -> ((f x), s))

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->
        let (x,s') = app st s in app (f x) s')