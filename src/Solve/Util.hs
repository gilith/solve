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
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showFFloat)

-------------------------------------------------------------------------------
-- Pretty print integers
-------------------------------------------------------------------------------

groupl :: Int -> [a] -> [[a]]
groupl k = foldl (\t h -> reverse h : t) [] . groupr k . reverse

groupr :: Int -> [a] -> [[a]]
groupr k = g . foldr f ((k,[]),[])
  where
    f x ((1,xs),xss) = ((k,[]), (x : xs) : xss)
    f x ((i,xs),xss) = ((i - 1, x : xs), xss)
    g ((_,[]),xss) = xss
    g ((_,xs),xss) = xs : xss

ppInteger :: Integral a => a -> String
ppInteger = ppSign . toInteger
  where
    ppSign i = if i < 0 then "-" ++ ppNat (-i) else ppNat i
    ppNat n = List.intercalate "," $ groupr 3 $ show n

ppHugeInteger :: Integral a => a -> String
ppHugeInteger = ppSign . toInteger
  where
    ppSign i = if i < 0 then "-" ++ ppNat (-i) else ppNat i
    ppNat n = let s = show n in
              let e = length s - 1 in
              List.intercalate "," (groupr 3 s) ++ " (~10^" ++ show e ++ ")"

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

isOneProb :: Prob -> Bool
isOneProb p = p >= 1.0

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

data Table =
    Table
      {borderTable :: Bool,
       alignLeftTable :: Bool,
       paddingTable :: Int}
  deriving (Show)

fmtTable :: Table -> [[String]] -> String
fmtTable fmt table = concatMap ppRow rows
  where
    rows :: [(Int,[(Int,[String])])]
    rows = map mkRow table

    colWidths :: [Int]
    colWidths = foldr (maxWidths . map fst . snd) [] rows

    cols :: Int
    cols = length colWidths

    mkRow :: [String] -> (Int,[(Int,[String])])
    mkRow [] = (0,[])
    mkRow row = (maximum (map (length . snd) ents), ents)
      where ents = map mkEntry row

    mkEntry :: String -> (Int,[String])
    mkEntry ent = case lines ent of
                    [] -> (0,[])
                    l -> (maximum (map length l), l)

    ppRow :: (Int,[(Int,[String])]) -> String
    ppRow (_,[]) = (if border then hBorder else "") ++ "\n"
    ppRow (h,ents) = concat ls
      where
        row = ents ++ replicate (cols - length ents) (0,[])
        (ls,_) = unfoldN peelRow h (zip colWidths row)

    peelRow :: [(Int,(Int,[String]))] -> (String, [(Int,(Int,[String]))])
    peelRow row = (l,row')
      where
        (row',(s,_)) = mapLR (peelEntry . vBorder) ("",0) row
        l = (if border then tail s else s) ++ "\n"

    peelEntry :: (String,Int) -> (Int,(Int,[String])) ->
                 ((Int,(Int,[String])),(String,Int))
    peelEntry (s,k) (cw,(ew,[])) = ((cw,(ew,[])), (s, k + cw + padding))
    peelEntry (s,k) (cw, (ew, x : xs)) = ((cw,(ew,xs)),sk)
      where
        sk = if alignLeft then skl else skr
        skl = (s ++ replicate k ' ' ++ x, (cw + padding) - xw)
        skr = (s ++ replicate ((k + cw) - ew) ' ' ++ x, (ew + padding) - xw)
        xw = length x

    vBorder :: (String,Int) -> (String,Int)
    vBorder (s,k) | border = (s ++ replicate k ' ' ++ "|", padding)
    vBorder (s,k) | otherwise = (s,k)

    hBorder :: String
    hBorder = tail $ concatMap sep colWidths
      where sep w = "+" ++ replicate (w + 2 * padding) '-'

    border :: Bool
    border = borderTable fmt

    alignLeft :: Bool
    alignLeft = alignLeftTable fmt

    padding :: Int
    padding = paddingTable fmt

    maxWidths :: [Int] -> [Int] -> [Int]
    maxWidths r1 r2 =
      zipWith max r1 r2 ++
      (case compare (length r1) (length r2) of
         LT -> drop (length r1) r2
         EQ -> []
         GT -> drop (length r2) r1)

ppTable :: [[String]] -> String
ppTable = fmtTable (Table True False 2)
