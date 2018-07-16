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

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Finding the first satisfying element of a list
-------------------------------------------------------------------------------

find :: (a -> Bool) -> [a] -> Maybe ([a],a,[a])
find p = go []
  where
    go _ [] = Nothing
    go xs (x : ys) = if p x then Just (reverse xs, x, ys) else go (x : xs) ys

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
-- Ordering and reordering
-------------------------------------------------------------------------------

orderBy :: (a -> a -> Ordering) -> [a] -> [(Int,a)]
orderBy cmp = sortBy cmp2 . zip [0..]
  where cmp2 (_,x) (_,y) = cmp x y

reorder :: [(Int,a)] -> [a]
reorder = map snd . sortBy (comparing fst)

-------------------------------------------------------------------------------
-- An integer nth root function [1] satisfying
--
--  0 < n /\ 0 <= k /\ p = nthRoot n k
-- ------------------------------------
--        p ^ n <= k < (p + 1) ^ n
--
-- 1. https://en.wikipedia.org/wiki/Nth_root_algorithm
-------------------------------------------------------------------------------

nthRoot :: Integer -> Integer -> Integer
nthRoot 1 k = k
nthRoot _ 0 = 0
nthRoot n k = if k < n then 1 else go (k `div` n)
  where
    go x = if x' >= x then x else go x'
      where
        x' = ((n - 1) * x + k `div` (x ^ (n - 1))) `div` n

-------------------------------------------------------------------------------
-- Updating elements of a set
-------------------------------------------------------------------------------

updateSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
updateSet f s = Set.foldr g [] s
  where
    g x l = map (flip Set.insert (Set.delete x s)) (f x) ++ l
