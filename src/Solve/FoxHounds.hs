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

import Solve.Game (Game)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- The code assumes the board size is even
boardSize :: Int
boardSize = 8

-------------------------------------------------------------------------------
-- Coordinates
-------------------------------------------------------------------------------

data Coord =
    Coord Int Int
  deriving (Eq,Ord)

instance Show Coord where
  show (Coord x y) = Char.chr (Char.ord 'a' + x) : show (y + 1)

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

-------------------------------------------------------------------------------
-- Positions
-------------------------------------------------------------------------------

data Pos =
    Pos
      {foxOnMove :: Bool,
       fox :: Coord,
       hounds :: Set Coord}
  deriving (Eq,Ord,Show)

initial :: Pos
initial =
    Pos
      {foxOnMove = True,
       fox = Coord (2 * (n `div` 2)) (boardSize - 1),
       hounds = Set.fromList (map (\x -> Coord (2 * x + 1) 0) [0..(n-1)])}
  where
    n = boardSize `div` 2

occupied :: Pos -> Coord -> Bool
occupied p c = c == fox p || Set.member c (hounds p)

empty :: Pos -> Coord -> Bool
empty p = not . occupied p

-------------------------------------------------------------------------------
-- Legal moves
-------------------------------------------------------------------------------

foxMove :: Pos -> [Pos]
foxMove p = map mk cl
  where
    mk c = p {foxOnMove = False, fox = c}
    cl = filter (empty p) (foxAdjacent (fox p))

houndsMove :: Pos -> [Pos]
houndsMove p = map mk (updateSet mv (hounds p))
  where
    mk hs = p {foxOnMove = True, hounds = hs}
    mv h = filter (empty p) (houndAdjacent h)

move :: Pos -> [Pos]
move p = if foxOnMove p then foxMove p else houndsMove p

gameOver :: Pos -> Bool
gameOver = null . move

-------------------------------------------------------------------------------
-- Position evaluations
-------------------------------------------------------------------------------

data Eval =
    FoxEscape Int
  | FoxCapture Int
  deriving (Eq,Show)

instance Ord Eval where
  compare (FoxEscape n1) (FoxEscape n2) = compare n2 n1
  compare (FoxEscape _) (FoxCapture _) = GT
  compare (FoxCapture _) (FoxEscape _) = LT
  compare (FoxCapture n1) (FoxCapture n2) = compare n1 n2

foxWin :: Eval
foxWin = FoxEscape 0

houndsWin :: Eval
houndsWin = FoxCapture 0

delay :: Eval -> Eval
delay (FoxEscape n) = FoxEscape (n + 1)
delay (FoxCapture n) = FoxCapture (n + 1)

eval :: Pos -> Either Eval ([Eval] -> Bool -> Eval)
eval p = if gameOver p then Left result else Right lift
  where
    m = foxOnMove p
    result = if m then houndsWin else foxWin
    lift es _ = delay (if m then maximum es else minimum es)

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

game :: Game Pos Eval
game = Game.Game {Game.move = move, Game.eval = eval}
