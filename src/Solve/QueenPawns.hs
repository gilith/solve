{- |
module: $Header$
description: Queen & Pawns
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.QueenPawns
where

import qualified Data.Char as Char
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Solve.Game (Eval(..),Game,Games,Player(..),Solve,Study,Val)
import qualified Solve.Game as Game
import Solve.Strategy (ProbWin,Strategy,StrategyFail)
import qualified Solve.Strategy as Strategy
import Solve.Util

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

boardSize :: Int
boardSize = 8

-------------------------------------------------------------------------------
-- Coordinates
-------------------------------------------------------------------------------

data Coord =
    Coord Int Int
  deriving (Eq,Ord)

instance Show Coord where
  show (Coord x y) = showFile ++ showRank
    where
      showFile = [Char.chr (Char.ord 'a' + x)]
      showRank = show (y + 1)

xCoord :: Coord -> Int
xCoord (Coord x _) = x

yCoord :: Coord -> Int
yCoord (Coord _ y) = y

onBoard :: Coord -> Bool
onBoard (Coord x y) =
    0 <= x && x < boardSize &&
    0 <= y && y < boardSize

darkSquare :: Coord -> Bool
darkSquare (Coord x y) = even (x + y)

-------------------------------------------------------------------------------
-- Vectors
-------------------------------------------------------------------------------

newtype Vector = Vector {unVector :: Coord}
  deriving (Eq,Ord)

instance Show Vector where
  show (Vector (Coord x y)) = show (x,y)

addVector :: Vector -> Vector -> Vector
addVector (Vector (Coord x1 y1)) (Vector (Coord x2 y2)) =
    Vector (Coord (x1 + x2) (y1 + y2))

negVector :: Vector -> Vector
negVector (Vector (Coord x y)) = Vector (Coord (-x) (-y))

northVector :: Vector
northVector = Vector (Coord 0 1)

eastVector :: Vector
eastVector = Vector (Coord 1 0)

southVector :: Vector
southVector = negVector northVector

westVector :: Vector
westVector = negVector eastVector

northWestVector :: Vector
northWestVector = addVector northVector westVector

northEastVector :: Vector
northEastVector = addVector northVector eastVector

southEastVector :: Vector
southEastVector = addVector southVector eastVector

southWestVector :: Vector
southWestVector = addVector southVector westVector

rookVectors :: [Vector]
rookVectors = [northVector,eastVector,southVector,westVector]

bishopVectors :: [Vector]
bishopVectors = [northEastVector,southEastVector,southWestVector,northWestVector]

queenVectors :: [Vector]
queenVectors = concat (zipWith doubleton rookVectors bishopVectors)

moveByVector :: Vector -> Coord -> Coord
moveByVector v = unVector . addVector v . Vector

moveAlongVector :: Vector -> Coord -> [Coord]
moveAlongVector v = takeWhile onBoard . tail . iterate (moveByVector v)

moveAlongVectors :: [Vector] -> Coord -> [[Coord]]
moveAlongVectors = flip (map . flip moveAlongVector)

-------------------------------------------------------------------------------
-- Position representations
-------------------------------------------------------------------------------

data PosRep =
    PosRep
      {queen :: Coord,
       pawns :: Set Coord}
  deriving (Eq,Ord)

instance Show PosRep where
  show p = "\n" ++ side ++ concat (map showRow (reverse inds)) ++ side
    where
      side = "+" ++ replicate boardSize '-' ++ "+\n"
      inds = [0..(boardSize-1)]
      showRow y = "|" ++ map (showEntry . flip Coord y) inds ++ "|\n"
      showEntry c =
          if c == queen p then 'Q'
          else if Set.member c (pawns p) then 'P'
          else if darkSquare c then '*'
          else ' '

initialRep :: PosRep
initialRep =
    PosRep
      {queen = Coord (n `div` 2) n,
       pawns = Set.fromList (map (flip Coord 1) [0..n])}
  where
    n = boardSize - 1

occupied :: PosRep -> Coord -> Bool
occupied p c = c == queen p || Set.member c (pawns p)

empty :: PosRep -> Coord -> Bool
empty p = not . occupied p

-------------------------------------------------------------------------------
-- Legal moves
-------------------------------------------------------------------------------

queenMove :: PosRep -> [PosRep]
queenMove p = concatMap mk (moveAlongVectors queenVectors (queen p))
  where
    mk [] = []
    mk (c : cs) = let p' = p {queen = c} in
                  if empty p c then p' : mk cs
                  else [p' {pawns = Set.delete c (pawns p)}]

pawnsMove :: PosRep -> [PosRep]
pawnsMove p = map mk (updateSet mv (pawns p))
  where
    mk cs = p {pawns = cs}
    mv c = if occupied p c' then []
           else c' : (if yCoord c == 1 && empty p c'' then [c''] else [])
      where
        c' = moveByVector northVector c
        c'' = moveByVector northVector c'

moveRep :: Player -> PosRep -> [PosRep]
moveRep Player1 p = pawnsMove p
moveRep Player2 p = queenMove p

-------------------------------------------------------------------------------
-- Position evaluations
-------------------------------------------------------------------------------

pawnsToMoveVictoryRep :: PosRep -> Bool
pawnsToMoveVictoryRep p = pawnCapture || any pawnPromote (Set.toList cs)
  where
    pawnCapture = Set.member qsw cs || Set.member qse cs
    pawnPromote c = yCoord c == boardSize - 2 && c /= qs
    cs = pawns p
    q = queen p
    qs = moveByVector southVector q
    qsw = moveByVector southWestVector q
    qse = moveByVector southEastVector q

-------------------------------------------------------------------------------
-- Positions
-------------------------------------------------------------------------------

type Idx = Int

newtype Pos = Pos {unPos :: Idx}
  deriving (Eq,Ord)

mkPos :: PosRep -> Pos
mkPos p = Pos $ packPawns packQueen
  where
    packQueen = let Coord x y = queen p in pack boardSize y x
    packPawns n = foldr packPawn n [0..(boardSize-1)]
    packPawn x = pack (boardSize - 1) $
        case filter ((== x) . xCoord) (Set.toList (pawns p)) of
          [] -> 0
          [Coord _ y] -> y
          _ : _ : _ -> error "multiple pawns on same file"
    pack k i n = n * k + i

destPos :: Pos -> PosRep
destPos n = PosRep {queen = Coord qx qy, pawns = Set.fromList (mapMaybe id cs)}
  where
    (cs,n') = mapLR unpackPawn (unPos n) [0..(boardSize-1)]
    (qy,qx) = unpack boardSize n'
    unpackPawn m x = let (y,m') = unpack (boardSize - 1) m in
                     (if y == 0 then Nothing else Just (Coord x y), m')
    unpack k m = (m `mod` k, m `div` k)

instance Show Pos where
  show = show . destPos

initial :: Pos
initial = mkPos initialRep

move :: Player -> Pos -> [Pos]
move pl = map mkPos . moveRep pl . destPos

pawnsToMoveVictory :: Pos -> Bool
pawnsToMoveVictory = pawnsToMoveVictoryRep . destPos

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

game :: Game Pos
game pl p =
    if null ps then Left (Game.winEval (Game.turn pl))
    else if pl == Player1 && pawnsToMoveVictory p then Left (Win Player1 1)
    else Right ps
  where
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

winningForQueen :: Player -> Pos -> Bool
winningForQueen = winningFor Player2

winningForPawns :: Player -> Pos -> Bool
winningForPawns = winningFor Player1

winDepth :: Player -> Pos -> Int
winDepth pl p =
    case Game.evalUnsafe solution pl p of
      Win _ d -> d
      Draw -> error "draws are not possible in this game"

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
-- Strategies
-------------------------------------------------------------------------------

stopLossStrategy :: Player -> Int -> Strategy Pos
stopLossStrategy = Strategy.stopLossStrategy solution

-------------------------------------------------------------------------------
-- Validating strategies
-------------------------------------------------------------------------------

validateStrategy :: Player -> Strategy Pos -> StrategyFail Pos
validateStrategy pl str =
    Strategy.validateStrategy game solution pl str Player1 initial

-------------------------------------------------------------------------------
-- Win probability
-------------------------------------------------------------------------------

probWin :: Player -> Strategy Pos -> ProbWin Pos
probWin pl adv = Strategy.probWin game pl adv Player1 initial

-------------------------------------------------------------------------------
-- The opposite position is reachable and has a different result
-------------------------------------------------------------------------------

opposite :: (Player,Pos)
opposite =
    case filter (opp . ev) bfsInitial of
      [] -> error "no opposite positions"
      p : _ -> p
  where
    opp = not . Game.sameResult (evalInitial solution)
    ev = uncurry $ Game.evalUnsafe solution

evalOpposite :: Val Pos v -> v
evalOpposite db = uncurry (Game.evalUnsafe db) opposite

-------------------------------------------------------------------------------
-- Typical reachable positions satisfying some predicate
-------------------------------------------------------------------------------

typical :: (Player -> Pos -> Bool) -> (Player,Pos)
typical f = middle $ filter (uncurry f) bfsInitial

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

instance Game.Printable Pos where
  ppPosition = tail . show

  ppPlayer _ Player1 = "Pawns"
  ppPlayer _ Player2 = "Queen"

ppPlayer :: Player -> String
ppPlayer = Game.ppPlayer initial

ppEval :: Eval -> String
ppEval = Game.ppEval initial
