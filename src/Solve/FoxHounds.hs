{- |
module: $Header$
description: Fox & Hounds
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.FoxHounds
where

import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set

import Solve.Game (Adversary,Game,Player(..),Solve)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

packSize :: Int
packSize = 4

-------------------------------------------------------------------------------
-- Coordinates
-------------------------------------------------------------------------------

data Coord =
    Coord Int Int
  deriving (Eq,Ord)

instance Show Coord where
  show (Coord x y) = Char.chr (Char.ord 'a' + x) : show (y + 1)

boardSize :: Int
boardSize = 2 * packSize

onBoard :: Coord -> Bool
onBoard (Coord x y) =
    0 <= x && x < boardSize &&
    0 <= y && y < boardSize

rankAdjacent :: Int -> Int -> [Coord]
rankAdjacent x y = filter onBoard [Coord (x - 1) y, Coord (x + 1) y]

foxAdjacent :: Coord -> [Coord]
foxAdjacent (Coord x y) = rankAdjacent x (y - 1) ++ rankAdjacent x (y + 1)

houndAdjacent :: Coord -> [Coord]
houndAdjacent (Coord x y) = rankAdjacent x (y + 1)

houndsReachable :: Set Coord -> Set Coord
houndsReachable = transitiveClosure houndAdjacent . Set.toList

foxReachable :: Set Coord -> Coord -> Set Coord
foxReachable hs =
    transitiveClosure unhounded . singleton
  where
    unhounded = filter (flip Set.notMember hs) . foxAdjacent

-------------------------------------------------------------------------------
-- Positions
-------------------------------------------------------------------------------

data Pos =
    Pos
      {fox :: Coord,
       hounds :: Set Coord}
  deriving (Eq,Ord,Show)

initial :: Pos
initial =
    Pos
      {fox = Coord (2 * (n `div` 2)) (boardSize - 1),
       hounds = Set.fromList (map (\x -> Coord (2 * x + 1) 0) [0..(n-1)])}
  where
    n = boardSize `div` 2

occupied :: Pos -> Coord -> Bool
occupied p c = c == fox p || Set.member c (hounds p)

empty :: Pos -> Coord -> Bool
empty p = not . occupied p

foxBox :: Pos -> Bool
foxBox p = Set.isSubsetOf f h
  where
    f = foxReachable (hounds p) (fox p)
    h = houndsReachable (hounds p)

-------------------------------------------------------------------------------
-- Legal moves
-------------------------------------------------------------------------------

foxMove :: Pos -> [Pos]
foxMove p = map mk cl
  where
    mk c = p {fox = c}
    cl = filter (empty p) (foxAdjacent (fox p))

houndsMove :: Pos -> [Pos]
houndsMove p = map mk (updateSet mv (hounds p))
  where
    mk hs = p {hounds = hs}
    mv h = filter (empty p) (houndAdjacent h)

move :: Player -> Pos -> [Pos]
move Player1 p = foxMove p
move Player2 p = houndsMove p

-------------------------------------------------------------------------------
-- Position evaluations
-------------------------------------------------------------------------------

foxEscaped :: Pos -> Bool
foxEscaped p = Set.notMember (fox p) (houndsReachable (hounds p))

won :: Player -> Pos -> Maybe Player
won pl p | null (move pl p) = Just (Game.turn pl)
won _ p | foxEscaped p = Just Player1
won _ _ | otherwise = Nothing

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

game :: Game Pos
game pl p =
    if null ps then Left (Game.win (Game.turn pl))
    else if foxEscaped p then Left (Game.win Player1)
    else Right ps
  where
    ps = move pl p

-------------------------------------------------------------------------------
-- Solution
-------------------------------------------------------------------------------

solution :: Solve Pos
solution = snd (Game.solve game Player1 initial)

-------------------------------------------------------------------------------
-- Adversaries
-------------------------------------------------------------------------------

stopLossAdversary :: Player -> Int -> Adversary Pos
stopLossAdversary = Game.stopLossAdversary solution

foxBoxAdversary :: Adversary Pos
foxBoxAdversary = Game.filterAdversary foxBox

-------------------------------------------------------------------------------
-- Win probability
-------------------------------------------------------------------------------

probWin :: Player -> Adversary Pos -> Prob
probWin pl adv = Game.probWin game pl adv Player1 initial
