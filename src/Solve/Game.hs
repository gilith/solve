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
import Data.Maybe (fromMaybe)

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

eval :: Ord p => Solve p -> Player -> p -> Maybe Eval
eval sol pl p = Map.lookup (pl,p) sol

evalUnsafe :: Ord p => Solve p -> Player -> p -> Eval
evalUnsafe sol pl p =
    case eval sol pl p of
      Just e -> e
      Nothing -> error "position is unreachable"

-------------------------------------------------------------------------------
-- Adversaries
-------------------------------------------------------------------------------

type Weight = Double

type Adversary p = [p] -> [Weight]

probAdversary :: Adversary p -> [p] -> [Prob]
probAdversary adv = normalize . adv

combineAdversary :: Adversary p -> Adversary p -> Adversary p
combineAdversary adv1 adv2 ps = zipWith (*) (adv1 ps) (adv2 ps)

orelseAdversary :: Adversary p -> Adversary p -> Adversary p
orelseAdversary adv1 adv2 ps =
    if sum w1 <= 0.0 then w2 else w1
  where
    w1 = adv1 ps
    w2 = adv2 ps

weightAdversary :: (p -> Weight) -> Adversary p
weightAdversary w = map w

uniformAdversary :: Adversary p
uniformAdversary = weightAdversary (const 1.0)

filterAdversary :: (p -> Bool) -> Adversary p
filterAdversary p = weightAdversary (b2w . p)
  where
    b2w True = 1.0
    b2w False = 0.0

stopLossAdversary :: Ord p => Solve p -> Player -> Int -> Adversary p
stopLossAdversary sol pl n = filterAdversary f
  where
    f p = let e = evalUnsafe sol pl p in not (better pl e ok)
    ok = Win pl n

-------------------------------------------------------------------------------
-- Compute probability of win
-------------------------------------------------------------------------------

probWin :: Ord p => Game p -> Player -> Adversary p -> Player -> p -> Prob
probWin game wpl adv = curry (fst . Graph.dfs pre post)
  where
    pre (pl,p) =
        case game pl p of
          Left e -> Left (if better wpl e Draw then 1.0 else 0.0)
          Right ps -> Right (map ((,) (turn pl)) (pruneZeroProb pl ps))

    post (pl,_) pws =
        if pl == wpl then maximum ws
        else mean (zip (probAdversary adv ps) ws)
      where
        (pps,mws) = unzip pws
        ps = map snd pps
        ws = map (fromMaybe 0.0) mws

    pruneZeroProb pl ps =
        if pl == wpl then ps
        else map snd $ filter (nonZeroProb . fst) $ zip (adv ps) ps
