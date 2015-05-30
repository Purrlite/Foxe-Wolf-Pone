import Data.Char (toLower)
import Data.List (stripPrefix)
import System.Environment (getArgs)

import Square
import Grid
import Player


main = do
    args <- getArgs
    let arg = normalizeArg . head $ args

    if null args || arg == "help" then
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
normalizeArg str = case (stripPrefix "--") . map toLower $ str of Just s  -> s
                                                                  Nothing -> str


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

    strs <- fmap words getLine
    let index1 = (read (strs !! 0)) :: Int
        index2 = (read (strs !! 1)) :: Int
        gIndex = (index1 - 1, index2 - 1)

    if isValidMove grid gIndex then
        return gIndex
    else do
        putStrLn $ (show (index1, index2)) ++ " isn't a valid move.\n"
        getValidMove player grid


isGameWon :: PlayingGrid -> Bool
isGameWon grid
    = any (\x -> all (== P1mark) x || all (== P2mark) x) squaresInAllDirs
    where squaresInAllDirs = map (map (getSquareAt grid)) allDirections
          allDirections = [
                -- Rows:
                [(0,0), (1,0), (2,0)],
                [(0,1), (1,1), (2,2)],
                [(0,2), (1,2), (2,2)],
                -- Columns:
                [(0,0), (0,1), (0,2)],
                [(1,0), (1,1), (1,2)],
                [(2,0), (2,1), (2,2)],
                -- Diagonals:
                [(0,0), (1,1), (2,2)],
                [(0,2), (1,1), (2,0)]
            ]


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
