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
			| otherwise = str ++ "|" ++ show point 


showEmptyLine :: String
showEmptyLine = replicate 5 '—'


showGrid :: PlayingGrid -> String
showGrid grid = foldl1 connectLines transformedLines
	where transformedLines = map showLine grid
	      connectLines line1 line2
	      		= line1 ++ "\n" ++ showEmptyLine ++ "\n" ++ line2


getSquareAt :: PlayingGrid -> GridIndex -> Square
grid `getSquareAt` (index1, index2) 
	| 0 <= index1 && index1 < 3 && 0 <= index2 && index2 < 3
			= grid !! index1 !! index2
	| otherwise
			= error "Index out of range." 


isSquare :: PlayingGrid -> GridIndex -> Square -> Bool
isSquare grid indexes square = grid `getSquareAt` indexes == square


isSquareEmpty :: PlayingGrid -> GridIndex -> Bool
isSquareEmpty grid indexes = isSquare grid indexes Empty

