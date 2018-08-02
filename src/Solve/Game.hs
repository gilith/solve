{- |
module: $Header$
description: Finite two-player games
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.Game
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Solve.Graph as Graph
import Solve.Util

-------------------------------------------------------------------------------
-- Players
-------------------------------------------------------------------------------

--
-- Player1 moves first
--
data Player =
    Player1
  | Player2
  deriving (Eq,Ord,Show)

turn :: Player -> Player
turn Player1 = Player2
turn Player2 = Player1

-------------------------------------------------------------------------------
-- Position evaluations
-------------------------------------------------------------------------------

data Eval =
    Win Player Int
  | Draw
  deriving (Eq,Show)

--
-- Best result for Player1 compares highest
--
instance Ord Eval where
  compare (Win Player1 n1) (Win Player1 n2) = compare n2 n1
  compare (Win Player1 _) _ = GT
  compare _ (Win Player1 _) = LT
  compare (Win Player2 n1) (Win Player2 n2) = compare n1 n2
  compare (Win Player2 _) _ = LT
  compare _ (Win Player2 _) = GT
  compare Draw Draw = EQ

better :: Ord a => Player -> a -> a -> Bool
better Player1 x y = x > y
better Player2 x y = x < y

best :: Ord a => Player -> [a] -> a
best Player1 = maximum
best Player2 = minimum

betterEval :: Player -> Eval -> Eval -> Bool
betterEval pl (Win pl1 _) (Win pl2 _) = pl1 == pl && pl2 /= pl
betterEval pl (Win pl1 _) Draw = pl1 == pl
betterEval pl Draw (Win pl2 _) = pl2 /= pl
betterEval _ Draw Draw = False

winning :: Player -> Eval -> Bool
winning pl e = betterEval pl e Draw

win :: Player -> Eval
win p = Win p 0

delay :: Eval -> Eval
delay (Win p n) = Win p (n + 1)
delay Draw = Draw

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

--
-- The list of legal moves must not be empty
--
type Game p = Player -> p -> Either Eval [p]

move :: Game p -> Player -> p -> [p]
move game pl p =
    case game pl p of
      Left _ -> []
      Right ps -> ps

-------------------------------------------------------------------------------
-- Depth-first search
-------------------------------------------------------------------------------

type DfsPre p a v = Player -> p -> Either v [(a,p)]

type DfsPost p a v = Player -> p -> [((a,p), Maybe v)] -> v

type DfsPos p v = Map (Player,p) v

dfs :: Ord p => DfsPre p a v -> DfsPost p a v -> Player -> p -> (v, DfsPos p v)
dfs pre post = curry (Graph.dfs pre' post')
  where
    pre' (pl,p) =
        case pre pl p of
          Left v -> Left v
          Right aps -> Right (map (addPl (turn pl)) aps)

    post' (pl,p) = post pl p . map delPl

    addPl pl (a,p) = (a,(pl,p))

    delPl ((a,(_,p)),v) = ((a,p),v)

-------------------------------------------------------------------------------
-- Game solution
-------------------------------------------------------------------------------

type Solve p = DfsPos p Eval

solve :: Ord p => Game p -> Player -> p -> (Eval, Solve p)
solve game = dfs pre post
  where
    pre pl p =
        case game pl p of
          Left v -> Left v
          Right ps -> Right (map ((,) ()) ps)

    post pl _ = delay . best pl . map (fromMaybe Draw . snd)

reachable :: Solve p -> Int
reachable = Map.size

eval :: Ord p => Solve p -> Player -> p -> Maybe Eval
eval sol pl p = Map.lookup (pl,p) sol

evalUnsafe :: Ord p => Solve p -> Player -> p -> Eval
evalUnsafe sol pl p =
    case eval sol pl p of
      Just e -> e
      Nothing -> error "position is unreachable"

-------------------------------------------------------------------------------
-- Strategies
-------------------------------------------------------------------------------

-- Weights are positive
type Weight = Double

-- Strategies can filter out positions and change weights
type Strategy p = [(Weight,p)] -> [(Weight,p)]

applyStrategy :: Strategy p -> [p] -> [(Weight,p)]
applyStrategy str = str . map ((,) 1.0)

noStrategy :: [p] -> [(Weight,p)]
noStrategy = map ((,) undefined)

idStrategy :: Strategy p
idStrategy = id

emptyStrategy :: Strategy p
emptyStrategy = const []

thenStrategy :: Strategy p -> Strategy p -> Strategy p
thenStrategy str1 str2 = str2 . str1

orelseStrategy :: Strategy p -> Strategy p -> Strategy p
orelseStrategy str1 str2 wps =
    case str1 wps of
      [] -> str2 wps
      wps' -> wps'

tryStrategy :: Strategy p -> Strategy p
tryStrategy = flip orelseStrategy idStrategy

filterStrategy :: (p -> Bool) -> Strategy p
filterStrategy f = filter (f . snd)

stopLossStrategy :: Ord p => Solve p -> Player -> Int -> Strategy p
stopLossStrategy sol pl n = filterStrategy f
  where
    f p = let e = evalUnsafe sol pl' p in not (better pl' e ok)
    ok = Win pl' n
    pl' = turn pl

-------------------------------------------------------------------------------
-- Validating strategies
-------------------------------------------------------------------------------

type StrategyFail p = Set ((Eval,p),(Eval,p),(Eval,p))

strategyFail :: Ord p => Game p -> Solve p -> Player -> Strategy p -> Player -> p -> StrategyFail p
strategyFail game sol spl str = \ipl -> fst . dfs pre post ipl
  where
    pre pl p =
        case game pl p of
          Left _ -> Left Set.empty
          Right ps ->
              case strategize pl ps of
                [] -> Left Set.empty  -- this strategy pruned away all moves
                wps -> Right wps

    post pl p pfs =
        if pl /= spl then fs
        else if betterEval pl (fst z) (fst n) then fs'
        else fs
      where
        fs = Set.unions (mapMaybe snd pfs)
        fs' = Set.insert (evalSol pl p, n, z) fs
        z = bestStr (move game pl p)
        n = bestStr (map (snd . fst) pfs)

    strategize pl = if pl == spl then applyStrategy str else noStrategy

    bestStr = best spl . map (evalSol (turn spl))

    evalSol pl p = (evalUnsafe sol pl p, p)

-------------------------------------------------------------------------------
-- Compute probability of win
-------------------------------------------------------------------------------

probWin :: Ord p => Game p -> Player -> Strategy p -> Player -> p -> Prob
probWin game wpl adv = \ipl -> fst . dfs pre post ipl
  where
    pre pl p =
        case game pl p of
          Left e -> Left (boolProb (winning wpl e))
          Right ps -> Right (strategize pl ps)

    post _ _ [] = error "no moves"
    post pl _ wpps =
        if pl == wpl then maximum ps
        else expectation (normalize ws) ps
      where
        (wps,mps) = unzip wpps
        ws = map fst wps
        ps = map (fromMaybe 0.0) mps

    strategize pl = if pl == wpl then noStrategy else applyStrategy adv
