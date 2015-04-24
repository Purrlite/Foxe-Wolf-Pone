import Data.List (transpose)

import Square
import Grid
import Player


main = do
    putStr greetings
    putEmptyLine
    putStrLn "You are here to play 3x3 Foxe Wolf Pone!"
    playGame P1 emptyGrid


greetings :: String
greetings = "Ladies and gentlemen, welcome to the game of century!\n" ++
            "=====================================================\n"


playGame :: Player -> PlayingGrid -> IO ()
playGame player grid = do
    putEmptyLine
    printGrid grid
    index <- getValidMove player grid
    let newGrid = setSquare grid index (getPlayerMark player)
    if isGridFull newGrid then
        sayBye newGrid "The game has ended in a tie!"
    else if isGameWon newGrid then
        sayBye newGrid $ (show player) ++ " has won the game!"
    else
        playGame (getOtherPlayer player) newGrid


getValidMove :: Player -> PlayingGrid -> IO GridIndex
getValidMove player grid = do
    putStrLn $ (show player) ++ " choose where to place your move (format X Y):"
    line <- getLine
    let strs = words line
        index1 = (read (strs !! 0)) :: Int
        index2 = (read (strs !! 1)) :: Int
        gIndex = (index1 - 1, index2 - 1)
    if isValidMove grid gIndex then
        return gIndex
    else do
        putStrLn $ (show (index1, index2)) ++ " isn't a valid move.\n"
        getValidMove player grid


isGameWon :: PlayingGrid -> Bool
isGameWon grid = isAnyLineWon P1mark grid || isAnyLineWon P2mark grid ||
                 isAnyLineWon P1mark tranGrid || isAnyLineWon P2mark tranGrid ||
                 isADiagonalWon P1mark grid || isADiagonalWon P2mark grid ||
                 isADiagonalWon P1mark tranGrid || isADiagonalWon P2mark tranGrid
    where tranGrid = transpose grid


isAnyLineWon :: Square -> PlayingGrid -> Bool
isAnyLineWon mark = or . map (all (== mark))


isADiagonalWon :: Square -> PlayingGrid -> Bool
isADiagonalWon mark grid = isSquareX grid (0, 0) mark &&
                           isSquareX grid (1, 1) mark &&
                           isSquareX grid (2, 2) mark


wrapInBlock :: String -> String
wrapInBlock str = replicate (length str + 6) '#' ++ "\n" ++
                 "#  " ++ str ++ "  #\n" ++
                 replicate (length str + 6) '#' ++ "\n"


sayBye :: PlayingGrid -> String -> IO ()
sayBye grid str = do
    putEmptyLine
    printGrid grid
    putEmptyLine
    putStr $ wrapInBlock str


putEmptyLine :: IO ()
putEmptyLine = putStr "\n"
