import Prelude hiding (putStr)
import Data.Char
import System.IO hiding (putStr)

-- 1.  Redefine putStr :: String -> IO () using a list comprehension and the library function
--     sequence_ :: [IO a] -> IO () .

putStr :: String -> IO ()
putStr xs = sequence_ [putChar x | x <- xs]


-- 2.  Using recursion, define a version of putBoard :: Board -> IO () that displays nim boards of
--     any size, rather than being specific to boards with just five rows of stars. Hint: first define an
--     auxiliary function that takes the current row number as an additional argument.

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow r n = do putStrLn (show r ++ ":" ++ concat (replicate n "* "))

auxPutBoard :: Int -> Board -> IO ()
auxPutBoard n []     = return ()
auxPutBoard n (x:xs) = do putRow n x
                          auxPutBoard (n+1) xs

putBoard :: Board -> IO ()
putBoard b = auxPutBoard 1 b


-- 3.  In a similar manner to the first exercise, redefine the generalised version of putBoard using a list
--     comprehension and sequence_ .

putBoardNew :: Board -> IO ()
putBoardNew b = sequence_ [putRow r n | (r,n) <- zip [1..] b]


-- 4.  Define an action adder :: IO () that reads a given number of integers from the keyboard, one
--     per line, and displays their sum. For example:
--     > adder
--     How many numbers? 5
--     1
--     3
--     5
--     7
--     9
--     The total is 25
--     Hint: start by defining an auxiliary function that takes the current total and how many numbers
--     remain to be read as arguments. You will also likely need to use the library functions read and
--     show .

getDigit :: IO Int
getDigit = do x <- getChar
              putChar '\n'
              if isDigit x then
                  return (digitToInt x)
              else
                  do putStrLn "ERRORR: Invalid digit"
                     getDigit

adder :: IO ()
adder = do putStr "How many numbers? "
           x <- getDigit 
           y <- actualAdder x 0
           putStrLn ("The total is :" ++ show y)

actualAdder :: Int -> Int -> IO Int
actualAdder 0 n = do return n
actualAdder x n = do a <- getDigit
                     actualAdder (x-1) (n+a)


-- 5.  Redefine adder using the function sequence:: [IO a] -> IO [a] that performs a list of
--     actions and returns a list of the resulting values.

adderNew :: IO ()
adderNew = do putStr "How many numbers? "
              x <- getDigit
              xs <- sequence (replicate x getDigit)
              putStrLn ("The total is :" ++ show (sum xs))


-- 6.  Using getCh , define an action readLine :: IO String that behaves in the same way as
--     getLine , except that it also permits the delete key to be used to remove characters. Hint: the delete
--     character is ’\DEL’ , and the control character for moving the cursor back one space is ’\b’ .

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

removeLast :: String -> String
removeLast [] = []
removeLast xs = init xs

readLine' :: String -> IO String
readLine' xs = do x <- getCh
                  if x == '\DEL' then
                      do putChar '\b'
                         putChar ' '
                         putChar '\b'
                         readLine' (removeLast xs)
                  else if x == '\n' then
                           do putChar x
                              return xs
                       else 
                           do putChar x
                              readLine' (xs ++ [x])

readLine :: IO String
readLine = readLine' []