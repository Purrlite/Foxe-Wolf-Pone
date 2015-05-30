module Player (
	Player(..),
	getPlayerMark,
	getOtherPlayer
) where

import Square


data Player = P1 | P2
	deriving (Eq, Bounded)

instance Show Player where
	show P1 = "Player 1"
	show P2 = "Player 2"


getPlayerMark :: Player -> Square
getPlayerMark P1 = P1mark
getPlayerMark P2 = P2mark


getOtherPlayer :: Player -> Player
getOtherPlayer P1 = P2
getOtherPlayer P2 = P1
