{-# LANGUAGE  DoAndIfThenElse #-}

import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.List (stripPrefix)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Square
import Grid
import Player


main = do
    args <- getArgs
    let arg = normalizeArg . head $ args

    if null args || arg == "help" then do
        putStrLn helpMessage
        exitSuccess
    else if not . isValidRendering $ arg then
        error "Bad/wrong argument passed to FoxeWolfPone."
    else do

    putStrLn greetings

    let rendering = getRendering arg
    playGame P1 rendering emptyGrid


helpMessage :: String
helpMessage = "This is a game that is a variation on the game TicTacToe.\n\
              \\n\
              \To run this game you need to use one of the following arguments:\n\
              \ - 'help' / '--help'\n\
              \     - Displays this help message.\n\
              \\n\
              \ - 'tictactoe' / '--tictactoe'\n\
              \     - Runs the game with the old TicTacToe characters.\n\
              \\n\
              \ - 'animal' / 'animals' / '--animal' / '--animals'\n\
              \     - Runs the game with the new animal characters.\n\
              \     - WARNING: Might crash if your font doesn't support the Unicode animal characters.\n"


greetings :: String
greetings = "Ladies and gentlemen, welcome to the game of century!\n\
            \=====================================================\n\
            \\n\
            \You are here to play 3x3 Foxe Wolf Pone!\n\
            \\n\
            \REMEMBER! In case you want to exit the game early, then just don't write anything at all after being prompted and press enter!\n"


normalizeArg :: String -> String
normalizeArg str = fromMaybe str (stripPrefix "--" . map toLower $ str)


playGame :: Player -> SquareRendering -> PlayingGrid -> IO ()
playGame player rendering grid = do
    putEmptyLine
    printGrid rendering grid

    index <- getValidMove player grid
    let newGrid = setSquare grid index (getPlayerMark player)

    if isGridFull newGrid then
        sayBye rendering newGrid "The game has ended in a tie!"
    else if isGameWon newGrid then
        sayBye rendering newGrid $ show player ++ " has won the game!"
    else
        playGame (getOtherPlayer player) rendering newGrid


getValidMove :: Player -> PlayingGrid -> IO GridIndex
getValidMove player grid = do
    putStrLn $ show player ++ " choose where to place your move (format X Y):"

    strs <- fmap words getLine

    if null strs then do
        putStrLn "Exiting.\n"
        exitSuccess
    else if length strs == 1 then do
        putStrLn "ERROR: You need to write 2 indexes!\n"
        getValidMove player grid
    else do

    let gIndex@(i1, i2) = (read (strs !! 0) - 1, read (strs !! 1) - 1)

    if isValidMove grid gIndex then
        return gIndex
    else do
        putStrLn $ "ERROR: " ++ show (i1 + 1, i2 + 1) ++ " isn't a valid move.\n"
        getValidMove player grid


wrapInBlock :: String -> String
wrapInBlock str = unlines [hashfulLine, middleLine, hashfulLine]
    where hashfulLine = replicate (length str + 6) '#'
          middleLine  = "#  " ++ str ++ "  #"


sayBye :: SquareRendering -> PlayingGrid -> String -> IO ()
sayBye rendering grid str = do
    putEmptyLine
    printGrid rendering grid
    putEmptyLine
    putStr $ wrapInBlock str


putEmptyLine :: IO ()
putEmptyLine = putStrLn ""
