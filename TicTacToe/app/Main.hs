{-
   Project: Tic-Tac-Toe
   Module: Main.hs
   Authors: Sara Gustavsson and William Sandkvist
   2018-08-18
-}
module Main where

import Game
import Data.List
import Test.HUnit


{- play board stone
   The main game loop of the program. It takes a board and a stone and lets the player of the current stone choose a square
   to place it in. If there are 4 stones in a vertical, horizontal or diagonal row then the player of said row wins and
   are returned to the main function. The loop also prints the board and registers the moves.
   SIDE EFFECTS: Prints strings to the console.
-}
play :: Board -> Stone -> IO ()

-- VARIANT: if (Game.checkWinX board2 stone 0 0 0 || Game.checkWinY board2 stone 0 || Game.checkWinXY board2 stone 0)
play board stone = do
   putStrLn "\n \n \n"
   Game.showBoard board
   if stone == X
      then putStrLn "Player X's turn\n"
      else putStrLn "Player O's turn\n"
   putStrLn "Choose a square (e.g. A4, b3, c4)"
   putStrLn "Type Q to return to the main menu."
   input <- getLine
   if (input == "Q" || input == "q")
      then do
         main
      else if not(checkInputX input && checkInputY input)
         then do
            putStrLn "\n\nInvalid input. Please, try again."
            play board stone
         else do
            let convertedInput = (stone, (charInput input), (integerInput input))
            if checkDuplicate convertedInput board
               then do
                  putStrLn "\n\nThere's already a stone on that square. Please try another one."
                  play board stone
               else do
                  let board2 = Game.registerMove board convertedInput
                  if (Game.checkWinX board2 stone 0 0 0 || Game.checkWinY board2 stone 0 || Game.checkWinXY board2 stone 0)
                     then do
                        Game.showBoard board2
                        putStrLn "\n\nPlayer "
                        print stone
                        putStrLn "Winner!"
                        main
                     else if (length board2 == 81)
                        then do
                           putStrLn "\n\nGame draw!"
                           main
                        else do
                           let stone2 = if stone == X
                                          then O
                                          else X
                           play board2 stone2


{- main
   The main game function. It asks if a player wants to start a game or quit the game. If the player wants to start a game,
   then the play function is called upon. If they want to quit then the function returns ().
   RETURNS: ()
   SIDE EFFECTS: Prints strings to the console.
-}
main:: IO()

-- VARIANT:
main = do
   putStrLn $ take 2 $ repeat '\n'
   putStrLn "Welcome to our TicTacToe game!\n"
   putStrLn "1) Start a game"
   putStrLn "2) Quit the game \n"
   input <- getLine
   if input == "1"
      then play [] X
      else if input == "2"
         then return()
         else do
            putStrLn "Invalid input, please type either '1' or '2'"


{- checkInputX xs
   checks to see if the given move input's x-coordinate is valid.
   RETURNS: True if the move is valid, otherwise false.
   EXAMPLES: checkInputX "c4" = True
             checkInputX "foo" = False
-}
checkInputX :: String -> Bool

checkInputX [] = False
checkInputX [x, _]
   | x == 'A' || x == 'a' = True
   | x == 'B' || x == 'b' = True
   | x == 'C' || x == 'c' = True
   | x == 'D' || x == 'd' = True
   | x == 'E' || x == 'e' = True
   | x == 'F' || x == 'f' = True
   | x == 'G' || x == 'g' = True
   | x == 'H' || x == 'h' = True
   | x == 'I' || x == 'i' = True
   |otherwise = False
checkInputX xs = False


{- checkInputY xs
   Checks to see if the given move input's y-coordinate-coordinate is valid.
   RETURNS: True if the move is valid, otherwise False.
   EXAMPLES: checkInputX "c4" = True
             checkInputX "foo" = False
-}
checkInputY :: String -> Bool

checkInputY [] = False
checkInputY [_, y]
   | y == '1' = True
   | y == '2' = True
   | y == '3' = True
   | y == '4' = True
   | y == '5' = True
   | y == '6' = True
   | y == '7' = True
   | y == '8' = True
   | y == '9' = True
   |otherwise = False
checkInputY xs = False


{- charInput xs
   Converts the given two character string to the appropriate x-coordinate.
   PRE:  The string must be of two charaters, where the first one is a letter A-I.
   RETURNS: An integer based of the letter.
   EXAMPLES: checkInputX "c4" = 3
             checkInputX "D3" = 4
-}
charInput :: String -> Integer

charInput [x, _]
   | x == 'A' || x == 'a' = 1
   | x == 'B' || x == 'b' = 2
   | x == 'C' || x == 'c' = 3
   | x == 'D' || x == 'd' = 4
   | x == 'E' || x == 'e' = 5
   | x == 'F' || x == 'f' = 6
   | x == 'G' || x == 'g' = 7
   | x == 'H' || x == 'h' = 8
   | x == 'I' || x == 'i' = 9


{- integerInput xs
   Converts the given two character string to the appropriate y-coordinate.
   PRE:  The string must be of two charaters, where the second one is a number 1 - 2.
   RETURNS: The given number as an integer
   EXAMPLES: checkInputX "c4" = 4
             checkInputX "D3" = 3
-}
integerInput :: [Char] -> Integer

integerInput [_, y]
   | y == '1' = 1
   | y == '2' = 2
   | y == '3' = 3
   | y == '4' = 4
   | y == '5' = 5
   | y == '6' = 6
   | y == '7' = 7
   | y == '8' = 8
   | y == '9' = 9


{- checkDuplicate s xs
   Checks if the given input's x- and y-coordinate has already been registered to the board.
   RETURNS: True if the given input is already in the board, otherwise False.
   EXAMPLES: checkDuplicate (X, 3, 4) [(Position O 1 1), (Position X 3 4)] = True
             checkDuplicate (X, 1, 1) [(Position O 1 1), (Position X 3 4)] = True
             checkDuplicate (X, 1, 2) [(Position O 1 1), (Position X 3 4)] = False
-}
checkDuplicate :: (Stone, Integer, Integer) -> Board -> Bool

-- VARIANT: length xs
checkDuplicate s [] = False
checkDuplicate (s, x, y) ((Position s' c r):xs)
   | x == c && y == r = True
   | otherwise = checkDuplicate (s, x, y) xs


{-
   Test cases
-}
mTest1 = TestCase (assertEqual "checkDuplicate" True (checkDuplicate ( X, 6, 5) [(Position O 3 4),(Position X 4 4),(Position X 5 5),(Position O 6 5),(Position X 6 6)]))

mTest2 = TestCase (assertEqual "checkInputX " True (checkInputX "c4"))

mTest3 = TestCase (assertEqual "checkInputY" False (checkInputY "foo"))

mTest4 = TestCase (assertEqual "charInput" 3 (charInput "c4"))

mTest5 = TestCase (assertEqual "integerInput" 8 (integerInput "d8"))

mTest6 = TestCase (assertEqual "checkDuplicate" True (checkDuplicate (X, 1, 1) [(Position O 1 1), (Position X 3 4)]))


performTests = runTestTT (TestList [mTest1,mTest2,mTest3,mTest4,mTest5,mTest6,gTest1,gTest2,gTest3,gTest4,gTest5,gTest6])
