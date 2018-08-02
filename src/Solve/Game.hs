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

import Data.List (partition)
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

betterEval :: Player -> Eval -> Eval -> Bool
betterEval pl (Win pl1 _) (Win pl2 _) = pl1 == pl && pl2 /= pl
betterEval pl (Win pl1 _) Draw = pl1 == pl
betterEval pl Draw (Win pl2 _) = pl2 /= pl
betterEval _ Draw Draw = False

best :: Ord a => Player -> [a] -> a
best Player1 = maximum
best Player2 = minimum

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

-------------------------------------------------------------------------------
-- Game solution
-------------------------------------------------------------------------------

type Solve p = Map (Player,p) Eval

solve :: Ord p => Game p -> Player -> p -> (Eval, Solve p)
solve game = curry (Graph.dfs pre post)
  where
    pre (pl,p) =
        case game pl p of
          Left v -> Left v
          Right ps -> Right (map ((,) (turn pl)) ps)

    post (pl,_) = delay . best pl . map (fromMaybe Draw . snd)

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

type Weight = Double

type Strategy p = [p] -> [Weight]

probStrategy :: Strategy p -> [p] -> [Prob]
probStrategy adv = normalize . adv

partitionZeroStrategy :: Strategy p -> [p] -> ([p],[p])
partitionZeroStrategy adv ps = (map snd zs, map snd ns)
  where (zs,ns) = partition (zeroProb . fst) $ zip (adv ps) ps

pruneZeroStrategy :: Strategy p -> [p] -> [p]
pruneZeroStrategy adv = snd . partitionZeroStrategy adv

combineStrategy :: Strategy p -> Strategy p -> Strategy p
combineStrategy adv1 adv2 ps = zipWith (*) (adv1 ps) (adv2 ps)

orelseStrategy :: Strategy p -> Strategy p -> Strategy p
orelseStrategy adv1 adv2 ps =
    if sum w1 <= 0.0 then w2 else w1
  where
    w1 = adv1 ps
    w2 = adv2 ps

weightStrategy :: (p -> Weight) -> Strategy p
weightStrategy w = map w

uniformStrategy :: Strategy p
uniformStrategy = weightStrategy (const 1.0)

filterStrategy :: (p -> Bool) -> Strategy p
filterStrategy p = weightStrategy (b2w . p)
  where
    b2w True = 1.0
    b2w False = 0.0

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
strategyFail game sol spl str = curry (fst . Graph.dfs pre post)
  where
    pre (pl,p) =
        case game pl p of
          Left _ -> Left Set.empty
          Right ps -> case prune pl ps of
                        Left (z,n) -> Left (Set.singleton (evalStr pl p, n, z))
                        Right ps' -> Right (map ((,) (turn pl)) ps')

    post _ = Set.unions . mapMaybe snd

    prune pl ps | pl /= spl = Right ps
    prune pl ps | otherwise =
        if null ns then Right ps  -- this strategy pruned away all moves
        else if null zs then Right ns
        else if betterEval pl (fst z) (fst n) then Left (z,n)
        else Right ns
      where
        z = bestStr zs
        n = bestStr ns
        (zs,ns) = partitionZeroStrategy str ps

    bestStr = best spl . map (evalStr (turn spl))

    evalStr pl p = (evalUnsafe sol pl p, p)

-------------------------------------------------------------------------------
-- Compute probability of win
-------------------------------------------------------------------------------

probWin :: Ord p => Game p -> Player -> Strategy p -> Player -> p -> Prob
probWin game wpl adv = curry (fst . Graph.dfs pre post)
  where
    pre (pl,p) =
        case game pl p of
          Left e -> Left (if better wpl e Draw then 1.0 else 0.0)
          Right ps -> Right (map ((,) (turn pl)) (prune pl ps))

    post _ [] = error "no moves"
    post (pl,_) pws =
        if pl == wpl then maximum ws
        else mean (zip (probStrategy adv ps) ws)
      where
        (pps,mws) = unzip pws
        ps = map snd pps
        ws = map (fromMaybe 0.0) mws

    prune pl ps = if pl == wpl then ps else pruneZeroStrategy adv ps
