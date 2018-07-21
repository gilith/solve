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
import Data.Maybe (fromMaybe)

import qualified Solve.Graph as Graph

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

-------------------------------------------------------------------------------
-- Compute probability of win
-------------------------------------------------------------------------------

type Prob = Double

type Adversary p = [p] -> [Prob]

meanAdversary :: Adversary p
meanAdversary ps = replicate n (1.0 / fromIntegral n)
  where
    n = length ps

probWin :: Ord p => Game p -> Player -> Adversary p -> Player -> p -> Prob
probWin game wpl adv = curry (fst . Graph.dfs pre post)
  where
    pre (pl,p) =
        case game pl p of
          Left e -> Left (if isWin e then 1.0 else 0.0)
          Right ps -> Right (map ((,) (turn pl)) ps)

    post (pl,_) pws =
        if pl == wpl then maximum ws
        else sum (zipWith (*) (adv ps) ws)
      where
        (pps,mws) = unzip pws
        ps = map snd pps
        ws = map (fromMaybe 0.0) mws

    isWin (Win pl _) = pl == wpl
    isWin _ = False
