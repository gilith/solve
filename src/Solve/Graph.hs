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
import Data.Sequence (ViewL(..),(><))
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set

import Solve.Util

-------------------------------------------------------------------------------
-- Depth-first search
-------------------------------------------------------------------------------

type DfsPre n a v = n -> Either v [(a,n)]

type DfsPost n a v = n -> [((a,n), Maybe v)] -> v

type DfsResult n v = Map n v

dfsWith :: Ord n => DfsPre n a v -> DfsPost n a v ->
           DfsResult n v -> n -> (v, DfsResult n v)
dfsWith pre post = go Set.empty
  where
    go br db n =
        case Map.lookup n db of
          Just v -> (v,db)
          Nothing -> (v, Map.insert n v db')
            where
              (v,db') = evalNode br db n (pre n)

    evalNode _ db _ (Left v) = (v,db)
    evalNode br db n (Right ans) = (post n nvs, db')
      where
        (nvs,db') = mapLR (evalChild (Set.insert n br)) db ans

    evalChild br db (a,n) | Set.member n br = (((a,n),Nothing),db)
    evalChild br db (a,n) | otherwise = (((a,n), Just v), db')
      where
        (v,db') = go br db n

dfs :: Ord n => DfsPre n a v -> DfsPost n a v -> n -> (v, DfsResult n v)
dfs pre post = dfsWith pre post Map.empty

eval :: Ord n => DfsResult n v -> n -> Maybe v
eval db n = Map.lookup n db

evalUnsafe :: Ord n => DfsResult n v -> n -> v
evalUnsafe db n =
    case eval db n of
      Just v -> v
      Nothing -> error "node not in database"

-------------------------------------------------------------------------------
-- Breadth-first search
-------------------------------------------------------------------------------

bfs :: Ord n => (n -> [n]) -> n -> [n]
bfs next = go Set.empty . Sequence.singleton
  where
    go s l = case Sequence.viewl l of
               EmptyL -> []
               h :< t -> if Set.member h s then go s t
                         else h : go s' l'
                           where
                             s' = Set.insert h s
                             l' = t >< Sequence.fromList (next h)
