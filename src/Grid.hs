module Grid (
	PlayingGrid,
	GridIndex,
	emptyGrid,
	showGrid,
	printGrid,
	getSquareAt,
	isSquareX,
	isSquareEmpty,
	isValidMove,
	setSquare,
	isGridFull
) where

import Data.List (intercalate, intersperse)

import Square
import Player


type PlayingGrid = [[Square]]

type GridIndex = (Int, Int)


emptyGrid :: PlayingGrid
emptyGrid = replicate 3 (replicate 3 Empty)


showLine :: SquareRendering -> [Square] -> String
showLine rendering = intercalate "\9474" . map (show' rendering)
-- \9474 is a vertical bar


emptyLine :: String
emptyLine = "\9472\9532\9472\9532\9472"
-- \9472 is horizontal bar
-- \9532 is something like + but full
-- so as a result this looks like something like "-+-+-"


showGrid :: SquareRendering -> PlayingGrid -> String
showGrid rendering = unlines . intersperse emptyLine . map (showLine rendering)


printGrid :: SquareRendering -> PlayingGrid -> IO ()
printGrid rendering = putStrLn . showGrid rendering


isIndexValid :: GridIndex -> Bool
isIndexValid (i1, i2) = (0 <= i1 && i1 <= 2) && (0 <= i2 && i2 <= 2)


getSquareAt :: PlayingGrid -> GridIndex -> Square
grid `getSquareAt` i@(index1, index2)
	| isIndexValid i = grid !! (2 - index2) !! index1
	| otherwise      = error "Index out of range."


isSquareX :: PlayingGrid -> GridIndex -> Square -> Bool
isSquareX grid gIndex value = grid `getSquareAt` gIndex == value


isSquareEmpty :: PlayingGrid -> GridIndex -> Bool
isSquareEmpty grid gIndex = isSquareX grid gIndex Empty


isValidMove :: PlayingGrid -> GridIndex -> Bool
isValidMove grid index = isIndexValid index && isSquareEmpty grid index


setSquare :: PlayingGrid -> GridIndex -> Square -> PlayingGrid
setSquare grid gIndex square
	| square == Empty             = error "Can't empty a square."
	| not . isIndexValid $ gIndex = error "Index out of range."
	| isSquareEmpty grid gIndex   = error "Trying to access already used square."
	| otherwise                   = changeSquare grid gIndex square


changeSquare :: PlayingGrid -> GridIndex -> Square -> PlayingGrid
changeSquare grid (i1, i2) sq
	= map (\(x, i) -> if i /= index then x else changeSquare' x i1 sq) (zip grid [0..])
	where index = 2 - i2


changeSquare' :: [Square] -> Int -> Square -> [Square]
changeSquare' squares index sq
	= map (\(x, i) -> if i /= index then x else sq) (zip squares [0..])


isGridFull :: PlayingGrid -> Bool
isGridFull = all (notElem Empty)
