{- |
module: $Header$
description: Noughts & Crosses
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.NoughtsCrosses
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Solve.Game (Eval(..),Game,Games,Player(..),Solve,Study,Val)
import qualified Solve.Game as Game

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

boardSize :: Int
boardSize = 3

-------------------------------------------------------------------------------
-- Coordinates
-------------------------------------------------------------------------------

data Coord =
    Coord Int Int
  deriving (Eq,Ord)

instance Show Coord where
  show (Coord x y) = Char.chr (Char.ord 'a' + x) : show (y + 1)

coords :: [Int]
coords = [0 .. (boardSize - 1)]

row :: Int -> [Coord]
row = flip map coords . flip Coord

column :: Int -> [Coord]
column = flip map coords . Coord

board :: Set Coord
board = Set.fromList $ concatMap row coords

winningLines :: [Set Coord]
winningLines =
    map (Set.fromList . row) coords ++
    map (Set.fromList . column) coords ++
    [Set.fromList $ map (\i -> Coord i i) coords] ++
    [Set.fromList $ map (\i -> Coord (boardSize - (i + 1)) i) coords]

containsWinningLine :: Set Coord -> Bool
containsWinningLine cs = any (flip Set.isSubsetOf cs) winningLines

-------------------------------------------------------------------------------
-- Positions
-------------------------------------------------------------------------------

data Pos =
    Pos
      {noughts :: Set Coord,
       crosses :: Set Coord}
  deriving (Eq,Ord)

instance Show Pos where
  show p = "\n" ++ concat (List.intersperse sep rs)
    where
      rs = map (joinRow . map showEntry . row) (reverse coords)
      joinRow r = " " ++ trimr (concat (List.intersperse " | " r)) ++ "\n"
      sep = "---" ++ concat (replicate (boardSize - 1) "+---") ++ "\n"
      trimr = List.dropWhileEnd Char.isSpace
      showEntry c = case occupying p c of
                      Just Player1 -> "O"
                      Just Player2 -> "X"
                      Nothing -> " "

initial :: Pos
initial = Pos {noughts= Set.empty, crosses = Set.empty}

occupation :: Player -> Pos -> Set Coord
occupation Player1 = noughts
occupation Player2 = crosses

free :: Pos -> Set Coord
free p = Set.difference board (Set.union (noughts p) (crosses p))

occupying :: Pos -> Coord -> Maybe Player
occupying p c =
    if Set.member c (noughts p) then Just Player1
    else if Set.member c (crosses p) then Just Player2
    else Nothing

occupied :: Pos -> Coord -> Bool
occupied p c = Maybe.isJust (occupying p c)

isFree :: Pos -> Coord -> Bool
isFree p = not . occupied p

-------------------------------------------------------------------------------
-- Legal moves
-------------------------------------------------------------------------------

occupy :: Player -> Coord -> Pos -> Pos
occupy Player1 c p = p {noughts = Set.insert c (noughts p)}
occupy Player2 c p = p {crosses = Set.insert c (crosses p)}

move :: Player -> Pos -> [Pos]
move pl p = map (flip (occupy pl) p) (Set.toList (free p))

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

game :: Game Pos
game pl p =
    if containsWinningLine (occupation pl' p) then Left (Game.winEval pl')
    else if null ps then Left Game.Draw
    else Right ps
  where
    pl' = Game.turn pl
    ps = move pl p

gameOver :: Player -> Pos -> Bool
gameOver = Game.gameOver game

evalInitial :: Val Pos v -> v
evalInitial db = Game.evalUnsafe db Player1 initial

bfsInitial :: [(Player,Pos)]
bfsInitial = Game.bfs game Player1 initial

-------------------------------------------------------------------------------
-- Solution
-------------------------------------------------------------------------------

solution :: Solve Pos
solution = Game.solve game Player1 initial

winningFor :: Player -> Player -> Pos -> Bool
winningFor wpl pl p = Game.winning wpl (Game.evalUnsafe solution pl p)

perfectPlay :: Player -> Pos -> [(Player,Pos)]
perfectPlay = Game.perfectPlay game solution

-------------------------------------------------------------------------------
-- The number of possible games
-------------------------------------------------------------------------------

games :: Games Pos
games = Game.games game Player1 initial

gamesInitial :: Integer
gamesInitial = evalInitial games

-------------------------------------------------------------------------------
-- Finding studies (sequences of only moves to win the game)
-------------------------------------------------------------------------------

study :: Player -> Study Pos
study spl = Game.study game solution spl Player1 initial

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

instance Game.Printable Pos where
  ppPosition = tail . show

  ppPlayer _ Player1 = "Noughts"
  ppPlayer _ Player2 = "Crosses"

ppPlayer :: Player -> String
ppPlayer = Game.ppPlayer initial

ppEval :: Eval -> String
ppEval = Game.ppEval initial
