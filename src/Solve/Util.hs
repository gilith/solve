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

import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showFFloat)

-------------------------------------------------------------------------------
-- Parity
-------------------------------------------------------------------------------

parity :: [Bool] -> Bool
parity = odd . length . filter id

-------------------------------------------------------------------------------
-- Making lists
-------------------------------------------------------------------------------

singleton :: a -> [a]
singleton x = [x]

doubleton :: a -> a -> [a]
doubleton x y = [x,y]

tripleton :: a -> a -> a -> [a]
tripleton x y z = [x,y,z]

-------------------------------------------------------------------------------
-- The middle element of a list
-------------------------------------------------------------------------------

middle :: [a] -> a
middle [] = error "no middle element of an empty list"
middle l = l !! ((length l - 1) `div` 2)

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
-- Unfolding lists
-------------------------------------------------------------------------------

unfold :: (a -> (b,a)) -> a -> [b]
unfold f = go
  where
    go s = x : go s' where (x,s') = f s

unfoldN :: (a -> (b,a)) -> Int -> a -> ([b],a)
unfoldN f = go []
  where
    go xs 0 s = (reverse xs, s)
    go xs n s = go (x : xs) (n - 1) s' where (x,s') = f s

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
-- Strings
-------------------------------------------------------------------------------

ucfirst :: String -> String
ucfirst [] = []
ucfirst (h : t) = Char.toUpper h : t

-------------------------------------------------------------------------------
-- Probabilities
-------------------------------------------------------------------------------

type Prob = Double

normalize :: [Double] -> [Prob]
normalize l = map (* c) l
  where c = 1.0 / sum l

expectation :: [Prob] -> [Double] -> Double
expectation pd = sum . zipWith (*) pd

isZeroProb :: Prob -> Bool
isZeroProb p = p <= 0.0

nonZeroProb :: Prob -> Bool
nonZeroProb = not . isZeroProb

boolProb :: Bool -> Prob
boolProb True = 1.0
boolProb False = 0.0

showProb :: Prob -> String
showProb p = showFFloat (Just 3) p ""

uniformDist :: Int -> [Prob]
uniformDist n = replicate n (1.0 / fromIntegral n)

sumDist :: Prob -> [Prob] -> [Prob] -> [Prob]
sumDist l = zipWith f
  where f p q = l * p + (1 - l) * q

fuzzDist :: Prob -> [Prob] -> [Prob]
fuzzDist e p = sumDist e (uniformDist (length p)) p

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
