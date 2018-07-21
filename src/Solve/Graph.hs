{- |
module: $Header$
description: Directed graphs
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.Graph
where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Solve.Util

-------------------------------------------------------------------------------
-- Depth-first search
-------------------------------------------------------------------------------

type DfsPre n v = n -> Either v [n]

type DfsPost n v = n -> [(n, Maybe v)] -> v

dfs :: Ord n => DfsPre n v -> DfsPost n v -> n -> (v, Map n v)
dfs pre post = go Set.empty Map.empty
  where
    go br db n =
        case Map.lookup n db of
          Just v -> (v,db)
          Nothing -> (v, Map.insert n v db')
            where
              (v,db') = evalNode br db n (pre n)

    evalNode _ db _ (Left v) = (v,db)
    evalNode br db n (Right ns) = (post n nvs, db')
      where
        (nvs,db') = mapLR (evalChild (Set.insert n br)) db ns

    evalChild br db n | Set.member n br = ((n,Nothing),db)
    evalChild br db n | otherwise = ((n, Just v), db')
      where
        (v,db') = go br db n
