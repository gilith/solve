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
import Data.List (sort)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Solve.Game (Eval(..),Event,Force,Game,Games,Max(..),Player(..),PlayerState(..),Solve,Val)
import qualified Solve.Game as Game
import Solve.Strategy (Adversaries,ProbWin,Strategy,StrategyFail)
import qualified Solve.Strategy as Strategy
import Solve.Util

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

boardSize :: Int
boardSize = 5

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

{-
coordToSquare :: Coord -> Idx
coordToSquare (Coord x y) = packSize * (boardSize - (y + 1)) + x `div` 2

squareToCoord :: Idx -> Coord
squareToCoord i = Coord x y
  where
    y = (boardSize - 1) - (i `div` packSize)
    x = 2 * (i `mod` packSize) + (1 - y `mod` 2)
-}

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
-- Positions
-------------------------------------------------------------------------------

type Idx = Int

data Pos =
    Pos
      {queen :: Coord,
       pawns :: Set Coord}
  deriving (Eq,Ord)

instance Show Pos where
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

initial :: Pos
initial =
    Pos
      {queen = Coord (n `div` 2) n,
       pawns = Set.fromList (map (flip Coord 1) [0..n])}
  where
    n = boardSize - 1

occupied :: Pos -> Coord -> Bool
occupied p c = c == queen p || Set.member c (pawns p)

empty :: Pos -> Coord -> Bool
empty p = not . occupied p

{-
posToIdx :: Pos -> Idx
posToIdx p = foldl pack 0 (f : hs)
  where
    pack n c = n * numSquares + c
    f = coordToSquare (fox p) + 1
    hs = sort $ map coordToSquare $ Set.toList $ hounds p

idxToPos :: Idx -> Pos
idxToPos i =
    Pos
      {fox = squareToCoord (f - 1),
       hounds = Set.fromList (map squareToCoord hs)}
  where
    unpack n = (n `mod` numSquares, n `div` numSquares)
    (hs,f) = unfoldN unpack packSize i
-}

-------------------------------------------------------------------------------
-- Legal moves
-------------------------------------------------------------------------------

queenMove :: Pos -> [Pos]
queenMove p = concatMap mk (moveAlongVectors queenVectors (queen p))
  where
    mk [] = []
    mk (c : cs) = let p' = p {queen = c} in
                  if empty p c then p' : mk cs
                  else [p' {pawns = Set.delete c (pawns p)}]

pawnsMove :: Pos -> [Pos]
pawnsMove p = map mk (updateSet mv (pawns p))
  where
    mk cs = p {pawns = cs}
    mv c = if occupied p c' then []
           else c' : (if yCoord c == 1 && empty p c'' then [c''] else [])
      where
        c' = moveByVector northVector c
        c'' = moveByVector northVector c'

move :: Player -> Pos -> [Pos]
move Player1 p = pawnsMove p
move Player2 p = queenMove p

-------------------------------------------------------------------------------
-- Position evaluations
-------------------------------------------------------------------------------

{-
foxEscaped :: Pos -> Bool
foxEscaped p = safe f && any safe (foxAdjacent f)
  where
    f = fox p
    safe = flip Set.notMember $ houndsReachable (hounds p)

won :: Player -> Pos -> Maybe Player
won pl p | null (move pl p) = Just (Game.turn pl)
won Player1 p | pawnsWon p = Just Player1
won _ _ | otherwise = Nothing
-}

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

game :: Game Pos
game pl p =
    if null ps then Left (Game.winEval (Game.turn pl))
    else if pl == Player1 && pawnsWin then Left (Win Player1 1)
    else Right ps
  where
    ps = move pl p
    pawnsWin = pawnCapture || any pawnPromote (Set.toList cs)
    pawnCapture = Set.member qsw cs || Set.member qse cs
    pawnPromote c = yCoord c == boardSize - 2 && c /= qs
    cs = pawns p
    q = queen p
    qs = moveByVector southVector q
    qsw = moveByVector southWestVector q
    qse = moveByVector southEastVector q

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

-------------------------------------------------------------------------------
-- The number of possible games
-------------------------------------------------------------------------------

games :: Games Pos
games = Game.games game Player1 initial

gamesInitial :: Integer
gamesInitial = evalInitial games

{-
-------------------------------------------------------------------------------
-- Strategies
-------------------------------------------------------------------------------

foxBox :: Force Pos
foxBox = Game.force game Player2 (const isFoxBox) Player1 initial

maxFoxBox :: Val Pos (Max Event)
maxFoxBox = Game.gameMax game Player1 (Game.evalUnsafe foxBox) Player1 initial

stopLossStrategy :: Player -> Int -> Strategy Pos
stopLossStrategy = Strategy.stopLossStrategy solution

foxBoxStrategy :: Int -> Strategy Pos
foxBoxStrategy = Strategy.forceStrategy foxBox Player2

maxFoxBoxStrategy :: Player -> Strategy Pos
maxFoxBoxStrategy = Strategy.maxStrategy . Game.evalUnsafe maxFoxBox

-- Best known parameterized strategies

foxStrategyN :: Int -> Strategy Pos
foxStrategyN n = Strategy.tryStrategy (stopLossStrategy Player1 n)

houndsStrategyN :: Int -> Strategy Pos
houndsStrategyN n =
    Strategy.thenStrategy
      (Strategy.tryStrategy (stopLossStrategy Player2 n))
      (Strategy.tryStrategy (foxBoxStrategy n))

adversaries :: Adversaries Pos
adversaries = PlayerState (mk houndsStrategyN, mk foxStrategyN)
  where mk sf = map (flip (,) Map.empty . sf) [0..]

-- Web game strategy

strategy :: Prob -> Player -> Strategy Pos
strategy fuzz pl =
    Strategy.mixedStrategy fuzz
      Strategy.idStrategy
      (Strategy.thenStrategy (Strategy.sameResultStrategy pl pe) str)
  where
    str [] = []
    str pws =
        (if Game.winning Player1 (pe $ fst $ head pws)
         then Strategy.bestStrategy Player2 pe
         else maxFoxBoxStrategy pl') pws
    pe = Game.evalUnsafe solution pl'
    pl' = Game.turn pl

moveDist :: Prob -> Player -> Pos -> [(Pos,Prob)]
moveDist fuzz pl p = Strategy.moveDistStrategy game (strategy fuzz pl) pl p

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
-}

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

ppPlayer :: Player -> String
ppPlayer Player1 = "Pawns"
ppPlayer Player2 = "Queen"

ppEval :: Eval -> String
ppEval (Win pl n) = ppPlayer pl ++ " win in " ++ show n
ppEval Draw = error "draws are impossible in this game"
