module Game where


import Data.List

data Stone = O | X | N
   deriving (Show, Eq)
data Player = Player | AI
   deriving (Show, Eq)

data Square = Position Stone Integer Integer
   deriving (Show, Eq)
type Board = [Square]


-- Prints the curent board
showBoard :: Board -> IO()

showBoard board = printFirstRow >> (printRows board 0 1)


-- Prints each row of the board
printRows :: Board -> Integer -> Integer -> IO()
printRows ((Position s x y):xs) x' y'
   | y' == 10 = putStrLn ""
   | x' == 0 = putStr "| " >> printNumber y' >> putStr " | " >> printRows ((Position s x y):xs) (x' + 1) y'
   | x' == 10 = putStrLn "" >> printRows ((Position s x y):xs) 0 (y' + 1)
   | x' == 0 = putStrLn "------------"
   | x == x' && y == y' = printStone s >> putStr " | " >> printRows xs (x' + 1) y'
   |otherwise = putStr "  | " >> printRows ((Position s x y):xs) (x' + 1) y'
printRows [] x' y'
   | y' == 10 = putStrLn ""
   | x' == 0 = putStr "| " >> printNumber y' >> putStr " | " >> printRows [] (x' + 1) y'
   | x' == 10 = putStrLn "" >> printRows [] 0 (y' + 1)
   |otherwise = putStr "  | " >> printRows [] (x' + 1) y'


-- Prints the first row
printFirstRow :: IO()

printFirstRow = putStrLn "|   | A | B | C | D | E | F | G | H | I |"


-- If you use print on a variable then putStrLn is used. This makes it so a nl is not created
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


--Same as the one above
printStone :: Stone -> IO()

printStone s
   | s == O = putStr "O"
   | s == X = putStr "X"


-- Adds a move to the appropriet place in the Board list
registerMove :: Board -> (Stone, Integer, Integer) -> Board

registerMove [] (c, x, y) = [(Position c x y)]
registerMove xs (c, x, y) = findRow xs (c, x, y)


--Since the Board list needs to be sorted by y over x (due to the showBoard function) findRow is used before findColumn
findRow :: Board -> (Stone, Integer, Integer) -> Board

findRow [] (c', x', y') = [Position c' x' y']
findRow ((Position c x y):xs) (c', x', y')
   | y < y' = [Position c x y]++(findRow xs (c', x', y'))
   | y > y' = [Position c' x' y']++[Position c x y]++xs
   | y == y' = findColumn ((Position c x y):xs) (c', x', y')


--Finds where to place the new square in the Board list
findColumn :: Board -> (Stone, Integer, Integer) -> Board

findColumn [] (c', x', y') = [Position c' x' y']
findColumn((Position c x y):xs) (c', x', y')
   | x < x' && y == y' = [Position c x y]++(findColumn xs (c', x', y'))
   | x < x' && y > y' = [Position c' x' y']++[Position c x y]++xs
   | x > x' = [Position c' x' y']++[Position c x y]++xs
   | otherwise = [Position c' x' y']++[Position c x y]++xs
