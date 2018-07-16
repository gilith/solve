{- |
module: $Header$
description: General games
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.Game
where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Solve.Util

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

data Game p e =
  Game
    {move :: p -> [p],
     eval :: p -> Either e ([e] -> Bool -> e)}

-------------------------------------------------------------------------------
-- Game solution
-------------------------------------------------------------------------------

solve :: Ord p => Game p e -> p -> (e, Map p e)
solve game = go Set.empty Map.empty
  where
    go g db p = (e, Map.insert p e db')
      where
        (e,db') = play g db p (eval game p)

    play _ db _ (Left e) = (e,db)
    play g db p (Right f) = (e,db')
      where
        g' = Set.insert p g
        cs = move game p
        ps = filter (flip Set.notMember g') cs
        (es,db') = mapLR (go g') db ps
        e = f es (length ps < length es)

