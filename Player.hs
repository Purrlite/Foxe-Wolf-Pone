module Player (
	Player(..),
	getPlayerMark
) where


data Player = P1 | P2
	deriving (Eq, Bounded)

instance Show Player where
	show P1 = "Player 1"
	show P2 = "Player 2"


getPlayerMark :: Player -> Square
getPlayerMark P1 = P1mark
getPlayerMark P2 = P2mark

