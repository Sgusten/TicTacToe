module Main where

import Game
import Data.List


--The main game loop
play :: Board -> Stone -> IO ()
play board stone = do
  Game.showBoard board
  if stone == X
    then putStrLn "Player X's turn\n"
    else putStrLn "Player O's turn\n"
  putStrLn "Choose a square (e.g. A4, b3, c4)"
  input <- getLine
  if not(checkInputX input && checkInputY input)
    then do
      putStrLn "Invalid input, please try again"
      play board stone
    else do
      let convertedInput = (stone, (charInput input), (integerInput input))
      let board2 = Game.registerMove board convertedInput
      let stone2 = if stone == X
          then O
          else X
      play board2 stone2

-- The executable function for the entire game
main:: IO()
main = do
   putStrLn $ take 2 $ repeat '\n'
   putStrLn "Welcome to our TicTacToe game!\n"
   putStrLn "1) Start a game"
   putStrLn "2) Quit \n"
   input <- getLine
   if input == "1"
      then play [] X
      else if input == "2"
	     then return()
         else do
            putStrLn "Invalid input, please type either '1' or '2'"


checkInputX :: String -> Bool

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


checkInputY :: String -> Bool

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


--Converts the given two character string to the appropriate x-coordinate
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


--converts the given two character string to the appropriate y-coordinate
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
