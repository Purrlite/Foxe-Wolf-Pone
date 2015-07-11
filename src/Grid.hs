{-# LANGUAGE  EmptyDataDecls #-}

module Grid (
	Grid(..),
		InUse,
		Won,
		Tied,
	GridIndex,
	Either3(..),
	emptyGrid,
	showGrid,
	printGrid,
	getSquareAt,
	isSquareX,
	isSquareEmpty,
	isValidMove,
	setSquare,
	isGridFull,
	isGridWon,
	finalizeGridIfGameFinished
) where

import Prelude hiding (Either(..))

import Data.List (intercalate, intersperse)

import Square
import Player


type Grid a = [[Square]]

data InUse
data Won
data Tied

type GridIndex = (Int, Int)


data Either3 a b c = Left a | Middle b | Right c


emptyGrid :: Grid a
emptyGrid = replicate 3 (replicate 3 Empty)


showLine :: SquareRendering -> [Square] -> String
showLine rendering = intercalate "\9474" . map (show' rendering)
-- \9474 is a vertical bar


emptyLine :: String
emptyLine = "\9472\9532\9472\9532\9472"
-- \9472 is horizontal bar
-- \9532 is something like + but full
-- so as a result this looks like something like "-+-+-"


showGrid :: SquareRendering -> Grid a -> String
showGrid rendering = unlines . intersperse emptyLine . map (showLine rendering)


printGrid :: SquareRendering -> Grid a -> IO ()
printGrid rendering = putStrLn . showGrid rendering


isIndexValid :: GridIndex -> Bool
isIndexValid (i1, i2) = (0 <= i1 && i1 <= 2) && (0 <= i2 && i2 <= 2)


getSquareAt :: Grid a -> GridIndex -> Square
grid `getSquareAt` i@(index1, index2)
	| isIndexValid i = grid !! (2 - index2) !! index1
	| otherwise      = error "Index out of range."


isSquareX :: Grid a -> GridIndex -> Square -> Bool
isSquareX grid gIndex value = grid `getSquareAt` gIndex == value


isSquareEmpty :: Grid a -> GridIndex -> Bool
isSquareEmpty grid gIndex = isSquareX grid gIndex Empty


isValidMove :: Grid InUse -> GridIndex -> Bool
isValidMove grid index = isIndexValid index && isSquareEmpty grid index


setSquare :: Grid InUse -> GridIndex -> Square -> Grid InUse
setSquare grid gIndex square
	| square == Empty                    = error "Can't empty a square."
	| not . isIndexValid $ gIndex        = error "Index out of range."
	| not . isSquareEmpty grid $ gIndex  = error "Trying to access already used square."
	| otherwise                          = changeSquare grid gIndex square


changeSquare :: Grid InUse -> GridIndex -> Square -> Grid InUse
changeSquare grid (i1, i2) sq
	= map (\(x, i) -> if i /= index then x else changeSquare' x i1 sq) (zip grid [0..])
	where index = 2 - i2


changeSquare' :: [Square] -> Int -> Square -> [Square]
changeSquare' squares index sq
	= map (\(x, i) -> if i /= index then x else sq) (zip squares [0..])


isGridWon :: Grid InUse -> Bool
isGridWon grid
    = any (\x -> all (== P1mark) x || all (== P2mark) x) squaresInAllDirs
    where squaresInAllDirs = map (map (getSquareAt grid)) allDirections
          allDirections = [
                -- Rows:
                [(0,0), (1,0), (2,0)],
                [(0,1), (1,1), (2,1)],
                [(0,2), (1,2), (2,2)],
                -- Columns:
                [(0,0), (0,1), (0,2)],
                [(1,0), (1,1), (1,2)],
                [(2,0), (2,1), (2,2)],
                -- Diagonals:
                [(0,0), (1,1), (2,2)],
                [(0,2), (1,1), (2,0)]
            ]


isGridFull :: Grid InUse -> Bool
isGridFull = all (notElem Empty)


finalizeGridIfGameFinished :: Grid InUse -> Either3 (Grid InUse) (Grid Tied) (Grid Won)
finalizeGridIfGameFinished grid
    | isGridWon grid  = Right grid
    | isGridFull grid = Middle grid
    | otherwise       = Left grid
