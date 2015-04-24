module Grid (
	PlayingGrid,
	GridIndex,
	emptyGrid,
	showGrid,
	printGrid,
	getSquareAt,
	isSquareX,
	isSquareEmpty,
	setSquare,
	isGridFull
) where

import Square
import Player


type PlayingGrid = [[Square]]

type GridIndex = (Int, Int)


emptyGrid :: PlayingGrid
emptyGrid = replicate 3 (replicate 3 Empty)


showLine :: [Square] -> String
showLine line = foldl connectPoints "" line
	where connectPoints str point
			| str == "" = str ++ show point
			| otherwise = str ++ "\9474" ++ show point
-- \9474 is a vertical bar


showEmptyLine :: String
showEmptyLine = "\9472\9532\9472\9532\9472"
-- \9472 is horizontal bar
-- \9532 is something like + but full
-- so as a result this looks like something like "-+-+-"


showGrid :: PlayingGrid -> String
showGrid grid = foldl1 connectLines transformedLines
	where transformedLines = map showLine grid
	      connectLines line1 line2
	      		= line1 ++ "\n" ++ showEmptyLine ++ "\n" ++ line2


printGrid :: PlayingGrid -> IO ()
printGrid = putStrLn . showGrid


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


setSquare :: PlayingGrid -> GridIndex -> Square -> PlayingGrid
setSquare grid gIndex square
	| square == Empty             = error "Can't empty a square."
	| not . isIndexValid $ gIndex = error "Index out of range."
	| isSquareEmpty grid gIndex   = changeSquare grid gIndex square
	| otherwise                   = error "Trying to access already used square."


changeSquare :: PlayingGrid -> GridIndex -> Square -> PlayingGrid
changeSquare grid (i1, i2) sq
	= map (\(x, i) -> if i /= index then x else changeSquare' x i1 sq) (zip grid [0..])
	where index = 2 - i2


changeSquare' :: [Square] -> Int -> Square -> [Square]
changeSquare' squares index sq
	= map (\(x, i) -> if i /= index then x else sq) (zip squares [0..])


isGridFull :: PlayingGrid -> Bool
isGridFull = and . map (all (/= Empty))
