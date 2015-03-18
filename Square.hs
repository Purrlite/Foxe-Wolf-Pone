module Square () where


data Square = Empty | P1mark | P2mark
	deriving (Eq, Bounded)

instance Show Square where
	show Empty  = "-"
	show P1mark = "X"
	show P2mark = "O"


