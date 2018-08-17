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
import Data.List (sort)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Solve.Game (Adversaries,Eval(..),Forced,Game,Player(..),PlayerState(..),ProbWin,Solve,Strategy,StrategyFail)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

packSize :: Int
packSize = 4

boardSize :: Int
boardSize = 2 * packSize

numSquares :: Int
numSquares = packSize * boardSize

-------------------------------------------------------------------------------
-- Coordinates
-------------------------------------------------------------------------------

type Idx = Int

data Coord =
    Coord Int Int
  deriving (Eq,Ord)

instance Show Coord where
  show (Coord x y) = Char.chr (Char.ord 'a' + x) : show (y + 1)

onBoard :: Coord -> Bool
onBoard (Coord x y) =
    0 <= x && x < boardSize &&
    0 <= y && y < boardSize &&
    (x + y) `mod` 2 == 1

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

coordParity :: Coord -> Bool
coordParity (Coord _ y) = y `mod` 2 == 1

coordToSquare :: Coord -> Idx
coordToSquare (Coord x y) = packSize * (boardSize - (y + 1)) + x `div` 2

squareToCoord :: Idx -> Coord
squareToCoord i = Coord x y
  where
    y = (boardSize - 1) - (i `div` packSize)
    x = 2 * (i `mod` packSize) + (1 - y `mod` 2)

-------------------------------------------------------------------------------
-- Positions
-------------------------------------------------------------------------------

data Pos =
    Pos
      {fox :: Coord,
       hounds :: Set Coord}
  deriving (Eq,Ord)

instance Show Pos where
  show p = "\n" ++ side ++ concat (map row (reverse inds)) ++ side
    where
      side = "+" ++ replicate boardSize '-' ++ "+\n"
      inds = [0..(boardSize-1)]
      row y = "|" ++ map (entry . flip Coord y) inds ++ "|\n"
      entry c =
        if c == fox p then 'F'
        else if Set.member c (hounds p) then 'H'
        else if onBoard c then '*'
        else ' '

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

posParity :: Pos -> Bool
posParity p = parity $ map coordParity (fox p : Set.toList (hounds p))

posToMove :: Pos -> Player
posToMove =
    \p -> if posParity p == ip then Player1 else Player2
  where
    ip = posParity initial

posToIdx :: Pos -> Idx
posToIdx p = foldl pack 0 (f : hs)
  where
    pack n c = n * numSquares + c
    f = coordToSquare $ fox p
    hs = sort $ map coordToSquare $ Set.toList $ hounds p

idxToPos :: Idx -> Pos
idxToPos i =
    Pos
      {fox = squareToCoord i',
       hounds = Set.fromList (map squareToCoord hs)}
  where
    unpack n = (n `mod` numSquares, n `div` numSquares)
    (hs,i') = unfoldN unpack packSize i

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

evalInitial :: Game.DfsResult Pos v -> v
evalInitial db = Game.evalUnsafe db Player1 initial

-------------------------------------------------------------------------------
-- Solution
-------------------------------------------------------------------------------

solution :: Solve Pos
solution = Game.solve game Player1 initial

-------------------------------------------------------------------------------
-- Strategies
-------------------------------------------------------------------------------

forcedFoxBox :: Forced Pos
forcedFoxBox = Game.forced game Player2 (const foxBox) Player1 initial

stopLossStrategy :: Player -> Int -> Strategy Pos
stopLossStrategy = Game.stopLossStrategy solution

foxBoxStrategy :: Int -> Strategy Pos
foxBoxStrategy = Game.forcedStrategy forcedFoxBox Player2

-- Best known strategies

foxStrategy :: Int -> Strategy Pos
foxStrategy n = Game.tryStrategy (stopLossStrategy Player1 n)

houndsStrategy :: Int -> Strategy Pos
houndsStrategy n =
    Game.thenStrategy
      (Game.tryStrategy (stopLossStrategy Player2 n))
      (Game.tryStrategy (foxBoxStrategy n))

adversaries :: Adversaries Pos
adversaries = PlayerState (mk houndsStrategy, mk foxStrategy)
  where mk sf = map (flip (,) Map.empty . sf . (* 2)) [0..]

-------------------------------------------------------------------------------
-- Validating strategies
-------------------------------------------------------------------------------

validateStrategy :: Player -> Strategy Pos -> StrategyFail Pos
validateStrategy pl str =
    Game.validateStrategy game solution pl str Player1 initial

-------------------------------------------------------------------------------
-- Win probability
-------------------------------------------------------------------------------

probWin :: Player -> Strategy Pos -> ProbWin Pos
probWin pl adv = Game.probWin game pl adv Player1 initial

moveDist :: Adversaries Pos -> Player -> Pos -> ([(Prob,Pos)], Adversaries Pos)
moveDist = Game.moveDist game solution

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

ppPlayer :: Player -> String
ppPlayer Player1 = "Fox"
ppPlayer Player2 = "Hounds"

ppEval :: Eval -> String
ppEval (Win pl n) = ppPlayer pl ++ " win in " ++ show n
ppEval Draw = error "draws are impossible in this game"
