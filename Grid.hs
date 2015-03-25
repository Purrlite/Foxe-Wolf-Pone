module Grid (
	PlayingGrid,
	GridIndex,
	emptyGrid,
	showGrid,
	getSquareAt,
	isSquare,
	isSquareEmpty
) where

import Square


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


isIndexValid :: GridIndex -> Bool
isIndexValid (i1, i2) = (0 <= i1 && i1 < 3) && (0 <= i2 && i2 < 3)


getSquareAt :: PlayingGrid -> GridIndex -> Square
grid `getSquareAt` i@(index1, index2)
	| isIndexValid i = grid !! index1 !! index2
	| otherwise      = error "Index out of range."


isSquare :: PlayingGrid -> GridIndex -> Square -> Bool
isSquare grid indexes square = grid `getSquareAt` indexes == square


isSquareEmpty :: PlayingGrid -> GridIndex -> Bool
isSquareEmpty grid indexes = isSquare grid indexes Empty

