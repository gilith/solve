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
import Numeric (showFFloat)

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

-------------------------------------------------------------------------------
-- Probabilities
-------------------------------------------------------------------------------

type Prob = Double

normalize :: [Double] -> [Prob]
normalize l = map (* c) l
  where c = 1.0 / sum l

mean :: [(Prob,Double)] -> Double
mean = sum . map (uncurry (*))

zeroProb :: Prob -> Bool
zeroProb p = p <= 0.0

nonZeroProb :: Prob -> Bool
nonZeroProb = not . zeroProb

showProb :: Prob -> String
showProb p = showFFloat (Just 3) p ""

-------------------------------------------------------------------------------
-- Pretty-print a table
-------------------------------------------------------------------------------

showTable :: [[String]] -> String
showTable rows =
    concat $ map (showRow (widths [] rows)) rows
  where
    showRow :: [Int] -> [String] -> String
    showRow ws [] =
        tail (concat (map (\w -> "+" ++ replicate (w + 2) '-') ws)) ++ "\n"
    showRow ws (c : cs) =
        drop 2 (showEntry (head ws) c) ++
        concat (zipWith showEntry (tail ws) cs) ++ "\n"

    showEntry :: Int -> String -> String
    showEntry w c = " | " ++ replicate (w - length c) ' ' ++ c

    widths :: [Int] -> [[String]] -> [Int]
    widths ws [] = ws
    widths ws (r : rs) = widths (combine ws (map length r)) rs

    combine :: [Int] -> [Int] -> [Int]
    combine r1 r2 =
      zipWith max r1 r2 ++
      (case compare (length r1) (length r2) of
         LT -> drop (length r1) r2
         EQ -> []
         GT -> drop (length r2) r1)
