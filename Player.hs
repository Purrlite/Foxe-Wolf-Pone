module Player (
	Player(..)
) where


data Player = P1 | P2
	deriving (Eq, Bounded, Show, Read)

