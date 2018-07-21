{- |
module: $Header$
description: Utility functions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.Util
where

import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Mapping with state over a list
-------------------------------------------------------------------------------

mapLR :: (s -> a -> (b,s)) -> s -> [a] -> ([b],s)
mapLR _ s [] = ([],s)
mapLR f s (x : xs) = (y : ys, s'')
  where
    (y,s') = f s x
    (ys,s'') = mapLR f s' xs

mapRL :: (a -> s -> (s,b)) -> [a] -> s -> (s,[b])
mapRL f = \xs s -> foldr g (s,[]) xs
  where
    g x (s,ys) = (s', y : ys) where (s',y) = f x s

-------------------------------------------------------------------------------
-- Updating elements of a set
-------------------------------------------------------------------------------

updateSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
updateSet f s = Set.foldr g [] s
  where
    g x l = map (flip Set.insert (Set.delete x s)) (f x) ++ l

-------------------------------------------------------------------------------
-- Transitive closure
-------------------------------------------------------------------------------

transitiveClosure :: Ord a => (a -> [a]) -> [a] -> Set a
transitiveClosure f = go Set.empty
  where
    go s [] = s
    go s (x : xs) | Set.member x s = go s xs
    go s (x : xs) | otherwise = go (Set.insert x s) (f x ++ xs)
