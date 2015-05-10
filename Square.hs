module Square (
	Square(..),
	SquareRendering(..),
	show',
	getRendering,
	isValidRendering
) where


data Square = Empty | P1mark | P2mark
	deriving (Eq, Bounded)



data SquareRendering = TicTacToe | Animal


show' _         Empty  = "-"
show' TicTacToe P1mark = "X"
show' TicTacToe P2mark = "O"
show' Animal    P1mark = "\128058"  -- Wolf face
show' Animal    P2mark = "\128052"  -- Horse face


getRendering :: String -> SquareRendering
getRendering "tictactoe" = TicTacToe
getRendering "animal"    = Animal
getRendering "animals"   = Animal


isValidRendering :: String -> Bool
isValidRendering "tictactoe" = True
isValidRendering "animal"    = True
isValidRendering "animals"   = True
isValidRendering _           = False
