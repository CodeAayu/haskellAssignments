{-
Game code from the chapter
-}

import Data.Char 
import Data.List 
import System.IO
import System.Random hiding (next)

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)


full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
              os = length (filter (==O) ps)
              xs = length (filter (==X) ps)
              ps = concat g  

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]


diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]


won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = 
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4)-1) '-']


showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside = foldr1 (zipWith (++))
                bar    = replicate 3 "|"



showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   ","   ","   "]
showPlayer X = ["   "," X ","   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys


valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B


move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)


getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

cls :: IO ()
cls = putStr "\ESC[2J"

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g   = putStrLn "Its a draw. \n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    [] -> do putStrLn "ERROR: Invalid Move"
                             run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "


data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
   | won g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2) -1)]]


prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]



depth :: Int 
depth = 9

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']


bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree


main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p 
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "Game ends in draw.\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of 
                      [] -> do putStrLn "ERROR: Invalid move"
                               play' g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking ..."
                   (play $! (bestmove g p)) (next p) 





-- 1.  Using the function gametree , verify that there are 549,946 nodes in the complete game tree for a
--     3×3 tic-tac-toe game starting from the empty grid, and that the maximum depth of this tree is 9.

totalNodes' :: Tree Grid -> Int
totalNodes' (Node _ []) = 1
totalNodes' (Node _ xs) = 1 + (sum . map totalNodes') xs

totalNodes :: Int
totalNodes = totalNodes' (gametree empty O)


-- 2.  Our tic-tac-toe program always chooses the first move from the list of best moves. Modify the final
--     program to choose a random move from the list of best moves, using the function randomRIO ::
--     (Int,Int) -> IO Int from System.Random to generate a random integer in the given range.

randomBestmove :: Grid -> Player -> IO Grid
randomBestmove g p = do temp <- randomRIO (0,length moves - 1)
                        return (moves !! temp)
                     where moves = [g' | Node (g',p') _ <- ts, p' == best]
                           tree = prune depth (gametree g p)
                           Node (_, best) ts = minimax tree

randomPlay' :: Grid -> Player -> IO ()
randomPlay' g p 
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "Game ends in draw.\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of 
                      [] -> do putStrLn "ERROR: Invalid move"
                               randomPlay' g p
                      [g'] -> randomPlay g' (next p)
   | p == X   = do putStr "Player X is thinking ..."
                   temp <- randomBestmove g p
                   (randomPlay $! (temp)) (next p)

randomPlay :: Grid -> Player -> IO ()
randomPlay g p = do cls
                    goto (1,1)
                    putGrid g
                    randomPlay' g p


-- 3.  Alternatively, modify the final program to choose a move that attempts to take the quickest route to
--     a win, by calculating the depths of resulting game trees and selecting a move that results in a tree
--     with the smallest depth.

treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 0
treeDepth (Node _ xs) = 1 + (minimum . map treeDepth) xs

fastBestmove :: Grid -> Player -> Grid
fastBestmove g p = head [g' | Node (g',p') _ <- sortOn treeDepth ts, p' == best]
               where
                  tree             = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree

fastPlay' :: Grid -> Player -> IO ()
fastPlay' g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "Game ends in draw.\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of 
                      [] -> do putStrLn "ERROR: Invalid move"
                               fastPlay' g p
                      [g'] -> fastPlay g' (next p)
   | p == X   = do putStr "Player X is thinking ..."
                   (fastPlay $! (fastBestmove g p)) (next p)

fastPlay :: Grid -> Player -> IO ()
fastPlay g p = do cls
                  goto (1,1)
                  putGrid g
                  fastPlay' g p


-- 4.  Modify the final program to:
--     a. let the user decide if they wish to play first or second

firstSelect :: String -> IO ()
firstSelect prompt = do putStr prompt
                        input <- getChar
                        if input == 'y'
                            then play empty O
                        else if input == 'n'
                            then play empty X
                        else 
                            do putStrLn "ERROR: Please only input 'y' or 'n'!"
                               firstSelect prompt

firstPlay :: IO()
firstPlay = firstSelect "Would you like to go first? (y/n): "



--     b. allow the length of a winning line to also be changed
--     c. generate the game tree once, rather than for each move
--     d. reduce the size of game tree using alpha-beta pruning

{-
Still an error in the whole algorithm when it starts with computer player.
Still debugging
-}