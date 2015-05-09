import Data.Char (toLower)
import Data.List (transpose)
import System.Environment (getArgs)

import Square
import Grid
import Player


main = do
    args <- getArgs
    let arg = normalizeArg . head $ args

    if length args == 0 || arg == "help" then
        putStrLn helpMessage
    else if not . isValidRendering $ arg then
        error "Bad/wrong argument passed to FoxeWolfPone."
    else do

    putStr greetings

    let rendering = getRendering arg
    playGame P1 rendering emptyGrid


helpMessage :: String
helpMessage
  = "This is a game that is a variation on the game TicTacToe.\n\
    \\n\
    \To run this game you need to use one of the following arguments:\n\
    \ - 'help'\n\
    \ - '--help'      - Displays this help message.\n\
    \\n\
    \ - 'tictactoe'\n\
    \ - '--tictactoe' - Runs the game with the old TicTacToe characters.\n\
    \\n\
    \ - 'animal'\n\
    \ - 'animals'\n\
    \ - '--animal'\n\
    \ - '--animals'   - Runs the game with the new animal characters.\n\
    \                   WARNING: Might crash if your font/terminal doesn't support the characters.\n"


greetings :: String
greetings = "Ladies and gentlemen, welcome to the game of century!\n\
            \=====================================================\n\
            \\n\
            \You are here to play 3x3 Foxe Wolf Pone!\n"


normalizeArg :: String -> String
normalizeArg = stripDashes . map toLower


stripDashes :: String -> String
stripDashes str = if str !! 0 == '-' && str !! 1 == '-' then
                  	drop 2 str
                  else
                  	str


playGame :: Player -> SquareRendering -> PlayingGrid -> IO ()
playGame player rendering grid = do
    putEmptyLine
    printGrid rendering grid

    index <- getValidMove player grid
    let newGrid = setSquare grid index (getPlayerMark player)

    if isGridFull newGrid then
        sayBye rendering newGrid "The game has ended in a tie!"
    else if isGameWon newGrid then
        sayBye rendering newGrid $ (show player) ++ " has won the game!"
    else
        playGame (getOtherPlayer player) rendering newGrid


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
                 isADiagonalWon P1mark grid || isADiagonalWon P2mark grid
    where tranGrid = transpose grid


isAnyLineWon :: Square -> PlayingGrid -> Bool
isAnyLineWon mark = or . map (all (== mark))


isADiagonalWon :: Square -> PlayingGrid -> Bool
isADiagonalWon mark grid = (isSquareX grid (0, 0) mark &&
                            isSquareX grid (1, 1) mark &&
                            isSquareX grid (2, 2) mark)
                           ||
                           (isSquareX grid (0, 2) mark &&
                            isSquareX grid (1, 1) mark &&
                            isSquareX grid (2, 0) mark)


wrapInBlock :: String -> String
wrapInBlock str = replicate (length str + 6) '#' ++ "\n" ++
                 "#  " ++ str ++ "  #\n" ++
                 replicate (length str + 6) '#' ++ "\n"


sayBye :: SquareRendering -> PlayingGrid -> String -> IO ()
sayBye rendering grid str = do
    putEmptyLine
    printGrid rendering grid
    putEmptyLine
    putStr $ wrapInBlock str


putEmptyLine :: IO ()
putEmptyLine = putStr "\n"
