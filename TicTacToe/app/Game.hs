{-
   Project: Tic-Tac-Toe
   Module: Game.hs
   Authors: Sara Gustavsson and William Sandkvist
   2018-08-18
-}

module Game where

import Data.List
import Test.HUnit


{- Represents the different players stones.
 -}
data Stone = O | X
   deriving (Show, Eq)

{- Represents the human player and the AI player.
 -}
data Player = Player | AI
   deriving (Show, Eq)

{- Represents a square on the game board that is not empty. Stone shows which player is on the square and the integers represents
   the x- and y-coordinates, or columnn and row, on the board.
 -}
data Square = Position Stone Integer Integer
   deriving (Show, Eq)

type Board = [Square]


{- showBoard board
   Prints the board in the console, which is 10x10 squares with letters A - I at the top and numbers 1-9 on the left.
   SIDE EFFECTS: Prints the board to the console.
-}
showBoard :: Board -> IO()

showBoard board = printFirstRow >> (printRows board 0 1)


{- printRows xs x' y'
   Prints the game board row by row, from left to right.
   SIDE EFFECTS: Prints the board to the console.
-}
printRows :: Board -> Integer -> Integer -> IO()

-- VARIANT: length xs
printRows ((Position s x y):xs) x' y'
   | y' == 10 = putStrLn ""
   | x' == 0 = putStr "| " >> printNumber y' >> putStr " | " >> printRows ((Position s x y):xs) (x' + 1) y'
   | x' == 10 = putStrLn "" >> printRows ((Position s x y):xs) 0 (y' + 1)
   | x == x' && y == y' = printStone s >> putStr " | " >> printRows xs (x' + 1) y'
   |otherwise = putStr "  | " >> printRows ((Position s x y):xs) (x' + 1) y'
printRows [] x' y'
   | y' == 10 = putStrLn ""
   | x' == 0 = putStr "| " >> printNumber y' >> putStr " | " >> printRows [] (x' + 1) y'
   | x' == 10 = putStrLn "" >> printRows [] 0 (y' + 1)
   |otherwise = putStr "  | " >> printRows [] (x' + 1) y'


{- printFirstRow
   Prints the first row, containing the letters A - I, to the console.
   SIDE EFFECTS: Prints "|   | A | B | C | D | E | F | G | H | I |" to the console
-}
printFirstRow :: IO()

printFirstRow = putStrLn "|   | A | B | C | D | E | F | G | H | I |"


{- printNumber n
   Converts an integer in to a string and prints it to the console. This is to avoind the new line created by using the print command.
   PRE:  The integer needs to be a number 1 - 9.
   SIDE EFFECTS: Prints the given integer to the screen.
-}
printNumber :: Integer -> IO()

printNumber n
   | n == 1 = putStr "1"
   | n == 2 = putStr "2"
   | n == 3 = putStr "3"
   | n == 4 = putStr "4"
   | n == 5 = putStr "5"
   | n == 6 = putStr "6"
   | n == 7 = putStr "7"
   | n == 8 = putStr "8"
   | n == 9 = putStr "9"


{- printStone s
   Converts a stone in to a string and prints it to the console. This is to avoid the new line created by using the print command.
   SIDE EFFECTS: Prints the given integer to the screen.
-}
printStone :: Stone -> IO()

printStone s
   | s == O = putStr "O"
   | s == X = putStr "X"


{- registerMove xs (c, x, y)
   Adds a move to the to the current board, and places it in the appropriet place in the list. The move is a tuple with the stone, x- and y-coordinates.
   RETURNS: A sorted board with the new move.
   EXAMPLES: registerMove [(Position X 2 3), (Position O 2 4)] (X, 3, 1) = [(Position X 3 1),(Position X 2 3),(Position O 2 4)]
-}
registerMove :: Board -> (Stone, Integer, Integer) -> Board

-- VARIANT: length xs
registerMove [] (c, x, y) = [(Position c x y)]
registerMove xs (c, x, y) = findRow xs (c, x, y)


{- findRow  xs (c, x, y)
   Finds the appropriet row to place the new move.
   RETURNS: A sorted board with the new move.
-}
findRow :: Board -> (Stone, Integer, Integer) -> Board

-- VARIANT: length xs
findRow [] (c', x', y') = [Position c' x' y']
findRow ((Position c x y):xs) (c', x', y')
   | y < y' = [Position c x y]++(findRow xs (c', x', y'))
   | y > y' = [Position c' x' y']++[Position c x y]++xs
   | y == y' = findColumn ((Position c x y):xs) (c', x', y')


{- findColumn xs (c, x, y)
   Finds the appropriet column to place the new move, and adds it to the list.
   RETURNS: A sorted board with the new move.
-}
findColumn :: Board -> (Stone, Integer, Integer) -> Board

-- VARIANT: length xs
findColumn [] (c', x', y') = [Position c' x' y']
findColumn((Position c x y):xs) (c', x', y')
   | x < x' && y == y' = [Position c x y]++(findColumn xs (c', x', y'))
   | x < x' && y > y' = [Position c' x' y']++[Position c x y]++xs
   | x > x' = [Position c' x' y']++[Position c x y]++xs
   | otherwise = [Position c' x' y']++[Position c x y]++xs


{- checkWinX xs s x y i
   Checks the board to see if there is a horizontal victory.
   PRE:  The integer, i, should not be the number 4 when the function is called for the first time.
   RETURNS: True if there is a horizontal victory, otherwise False.
   EXAMPLES: checkWinX [(Position X 1 1),(Position X 2 1),(Position X 3 1),(Position X 4 1),(Position O 2 2),Position O 3 2),(Position O 2 3)] X 0 0 0 = True
             checkWinX [(Position X 1 1),(Position X 2 1),(Position X 3 1),(Position X 4 1),(Position O 2 2),Position O 3 2),(Position O 2 3)] O 0 0 0 = False
-}
checkWinX :: Board -> Stone -> Integer -> Integer -> Integer -> Bool

-- VARIANT: length xs
checkWinX xs s' x y 4 = True
checkWinX [] s' x y ix = False
checkWinX ((Position s c r):xs) s' x y ix
   | s /= s' = checkWinX xs s' c r 0
   | y /= r && s == s' = checkWinX xs s' c r 1
   | y == r && (x + 1) == c = checkWinX xs s' c y (ix + 1)
   | otherwise = checkWinX xs s' c r 1


{- checkWinY xs s i
   Checks the board to see if there is a vertical victory.
   PRE:  The integer, i, should not be the number 4 when the function is called for the first time.
   RETURNS: True if there is a vertical victory, otherwise False
   EXAMPLES: checkWinY [(Position X 1 1),(Position X 1 2),(Position O 2 2),(Position O 3 2),(Position X 1 3),(Position O 2 3),(Position X 1 4)] X 0 = True
             checkWinY [(Position X 1 2),(Position O 2 2),(Position O 3 2),(Position X 1 3),(Position O 2 3),(Position X 1 4)] O 0 = False
-}
checkWinY :: Board -> Stone -> Integer -> Bool

-- VARIANT: length xs
checkWinY xs s 4 = True
checkWinY [] s i = False
checkWinY ((Position s c r):xs) s' i
   | s == s' = checkWinY xs s' (checkVertical xs s' c r 1)
   | otherwise = checkWinY xs s' i


{- checkVertical xs s y i
   Checks each square to see if they make a vertical victory.
   PRE:  The integer, i, should not be the number 4 when the function is called for the first time.
   RETURNS: 4 if there is a vertical victory, otherwise 0.
-}
checkVertical :: Board -> Stone -> Integer -> Integer -> Integer -> Integer

-- VARIANT: length xs
checkVertical xs s x y 4 = 4
checkVertical [] s x y i = 0
checkVertical ((Position s c r):xs) s' x y i
   | c == x && s == s' && r == (y + 1) = checkVertical xs s' c r (i + 1)
   | c == x && s /= s' = checkVertical xs s' c r 0
   | c == x && s == s = checkVertical xs s' c r 1
   | otherwise = checkVertical xs s' x y i


{- checkWinXY xs s i
   Checks the board to see if there is a diagonal victory.
   PRE:  The integer, i, should not be the number 4 when the function is called for the first time.
   RETURNS: True if there is a diagonal victory, otherwise False.
   EXAMPLES: checkWinXY [(Position X 3 3),(Position O 3 4),(Position X 4 4),(Position X 5 5),(Position O 6 5),(Position X 6 6)] X 0 = True
             checkWinXY [(Position X 3 3),(Position O 3 4),(Position X 4 4),(Position X 5 5),(Position O 6 5),(Position X 6 6)] O 0 = False
-}
checkWinXY :: Board -> Stone -> Integer -> Bool

-- VARIANT: length xs
checkWinXY xs s 4 = True
checkWinXY [] s i = False
checkWinXY ((Position s c r):xs) s' i
   | s == s' = checkWinXY xs s' (checkDiagonal xs s c r 1)
   | otherwise = checkWinXY xs s' i


{- checkDiagonal xs s x y i
   Checks each square to see if they make a diagonal victory.
   PRE:  The integer, i, should not be the number 4 when the function is called for the first time.
   RETURNS: 4 if there is a diagonal victory, otherwise 0.
-}
checkDiagonal :: Board -> Stone -> Integer -> Integer -> Integer -> Integer

-- VARIANT: length xs
checkDiagonal xs s x y 4 = 4
checkDiagonal [] s x y i = 0
checkDiagonal ((Position s c r):xs) s' x y i
   | s == s' && c == (x + 1) && r == (y + 1) = checkDiagonal xs s' c r (i + 1)
   | s /= s' && c == (x + 1) && r == (y + 1) = checkDiagonal xs s' c r 0
   | s == s' && c == (x - 1) && r == (y + 1) = checkDiagonal xs s' c r (i + 1)
   | s /= s' && c == (x - 1) && r == (y + 1) = checkDiagonal xs s' c r 0
   |otherwise = checkDiagonal xs s' x y i



-- Test cases
gTest1 = TestCase (assertEqual "registerMove" [(Position X 3 1),(Position X 2 3),(Position O 2 4)] (registerMove [(Position X 2 3), (Position O 2 4)] (X, 3, 1)))

gTest2 = TestCase (assertEqual "checkWinX" False (checkWinX [(Position X 1 1),(Position X 2 1),(Position X 3 1),(Position X 4 1),(Position O 2 2),(Position O 3 2),(Position O 2 3)] O 0 0 0))

gTest3 = TestCase (assertEqual "checkWinY" True (checkWinY [(Position X 1 1),(Position X 1 2),(Position O 2 2),(Position O 3 2),(Position X 1 3),(Position O 2 3),(Position X 1 4)] X 0))

gTest4 = TestCase (assertEqual "checkVertical" 4 (checkVertical [(Position X 1 2),(Position O 2 2),(Position O 3 2),(Position X 1 3),(Position O 2 3),(Position X 1 4)] X 1 1 1))

gTest5 = TestCase (assertEqual "checkWinXY" True (checkWinXY [(Position X 3 3),(Position O 3 4),(Position X 4 4),(Position X 5 5),(Position O 6 5),(Position X 6 6)] X 0))

gTest6 = TestCase (assertEqual "checkDiagonal" 4 (checkDiagonal [(Position O 3 4),(Position X 4 4),(Position X 5 5),(Position O 6 5),(Position X 6 6)] X 3 3 1))
