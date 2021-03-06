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
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Solve.Game (Eval(..),Event,Force,Game,Games,Max(..),Player(..),PlayerState(..),Solve,Study,Val)
import qualified Solve.Game as Game
import Solve.Strategy (Adversaries,ProbWin,Strategy,StrategyFail)
import qualified Solve.Strategy as Strategy
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

isFoxBox :: Pos -> Bool
isFoxBox p = Set.size f == 1 || Set.isSubsetOf f h
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
foxEscaped p = safe f && any safe (foxAdjacent f)
  where
    f = fox p
    safe = flip Set.notMember $ houndsReachable (hounds p)

won :: Player -> Pos -> Maybe Player
won pl p | null (move pl p) = Just (Game.turn pl)
won _ p | foxEscaped p = Just Player1
won _ _ | otherwise = Nothing

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

game :: Game Pos
game pl p =
    if null ps then Left (Game.winEval (Game.turn pl))
    else if foxEscaped p then Left (Game.winEval Player1)
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

winningForFox :: Player -> Pos -> Bool
winningForFox = winningFor Player1

winningForHounds :: Player -> Pos -> Bool
winningForHounds = winningFor Player2

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

foxBox :: Force Pos
foxBox = Game.force game Player2 isWinningFoxBox Player1 initial
  where
    isWinningFoxBox pl p = winningForHounds pl p && isFoxBox p

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

  ppPlayer _ Player1 = "Fox"
  ppPlayer _ Player2 = "Hounds"

ppPlayer :: Player -> String
ppPlayer = Game.ppPlayer initial

ppEval :: Eval -> String
ppEval = Game.ppEval initial
