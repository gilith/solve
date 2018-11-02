{- |
module: $Header$
description: Strategies
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.Strategy
where

import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe,isJust,mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import Solve.Game (Eval(..),Event(..),Force,Game,Moves,Player(..),PlayerState(..),Solve,Val)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Strategies
-------------------------------------------------------------------------------

-- Weights are positive
type Weight = Double

-- Strategies may filter out positions and change weights
type Strategy p = [(p,Weight)] -> [(p,Weight)]

moveDistStrategy :: Eq p => Game p -> Strategy p -> Player -> p -> [(p,Prob)]
moveDistStrategy game str pl p =
    case game pl p of
      Left _ -> []
      Right ps -> distStrategy str ps

distStrategy :: Eq p => Strategy p -> [p] -> [(p,Prob)]
distStrategy str ps = map pdf ps
  where
    pdf p = case filter ((== p) . fst) pps of
              [] -> (p,0.0)
              pp : _ -> pp
    (ps',ws) = unzip $ applyStrategy str ps
    pps = zip ps' (normalize ws)

applyStrategy :: Strategy p -> [p] -> [(p,Weight)]
applyStrategy str ps =
    case str $ map (flip (,) 1.0) ps of
      [] -> error "strategy pruned away all moves"
      pws -> pws

weightlessStrategy :: [p] -> [(p,Weight)]
weightlessStrategy = map (flip (,) undefined)

-------------------------------------------------------------------------------
-- Strategy combinators
-------------------------------------------------------------------------------

idStrategy :: Strategy p
idStrategy = id

noStrategy :: Strategy p
noStrategy = const []

thenStrategy :: Strategy p -> Strategy p -> Strategy p
thenStrategy str1 str2 = str2 . str1

orelseStrategy :: Strategy p -> Strategy p -> Strategy p
orelseStrategy str1 str2 pws =
    case str1 pws of
      [] -> str2 pws
      pws' -> pws'

tryStrategy :: Strategy p -> Strategy p
tryStrategy = flip orelseStrategy idStrategy

filterStrategy :: (p -> Bool) -> Strategy p
filterStrategy f = filter (f . fst)

maxStrategy :: Ord v => (p -> v) -> Strategy p
maxStrategy _ [] = []
maxStrategy pv ps = mapMaybe f $ zip ps vs
  where
    f (p,w) = if w == v then Just p else Nothing
    vs = map (pv . fst) ps
    v = maximum vs

bestStrategy :: Player -> (p -> Eval) -> Strategy p
bestStrategy Player1 pe = maxStrategy pe
bestStrategy Player2 pe = maxStrategy (Game.turnEval . pe)

sameResultStrategy :: Player -> (p -> Eval) -> Strategy p
sameResultStrategy pl pe = bestStrategy pl (squash . pe)
  where
    squash (Win wpl _) = Win wpl 0
    squash Draw = Draw

stopLossStrategy :: Ord p => Solve p -> Player -> Moves -> Strategy p
stopLossStrategy sol pl n = filterStrategy f
  where
    f p = let e = Game.evalUnsafe sol pl' p in not (Game.betterEval pl' e ok)
    ok = Win pl' n
    pl' = Game.turn pl

forceStrategy :: Ord p => Force p -> Player -> Moves -> Strategy p
forceStrategy frc pl n = filterStrategy f
  where f = (>) (In n) . Game.evalUnsafe frc (Game.turn pl)

mixedStrategy :: Ord p => Prob -> Strategy p -> Strategy p -> Strategy p
mixedStrategy p str1 str2 pws =
    if isZeroProb p then str2 pws
    else if isZeroProb q then str1 pws
    else Map.toList $ Map.unionWith (+) pws1 pws2
  where
    pws1 = Map.fromList $ map (scale p) $ str1 pws
    pws2 = Map.fromList $ map (scale q) $ str2 pws
    scale x (a,w) = (a, x * w)
    q = 1.0 - p

-------------------------------------------------------------------------------
-- Validating strategies
-------------------------------------------------------------------------------

type StrategyFail p = Set ((p,Eval),(p,Eval),(p,Eval))

validateStrategy :: Ord p => Game p -> Solve p -> Player -> Strategy p -> Player -> p -> StrategyFail p
validateStrategy game sol spl str = \ipl ->
    fst . Game.dfsWith pre post Map.empty ipl
  where
    pre pl p =
        case game pl p of
          Left _ -> Left Set.empty
          Right ps -> Right (map swap $ strategize pl ps)

    post pl p pfs =
        if pl /= spl then fs
        else if Game.betterResult pl (snd z) (snd n) then fs'
        else fs
      where
        fs = Set.unions (mapMaybe snd pfs)
        fs' = Set.insert (evalSol pl p, n, z) fs
        z = bestStr (Game.move game pl p)
        n = bestStr (map (snd . fst) pfs)

    strategize pl = if pl == spl then applyStrategy str else weightlessStrategy

    bestStr = maximumBy (Game.compareEval spl `on` snd) .
              map (evalSol (Game.turn spl))

    evalSol pl p = (p, Game.evalUnsafe sol pl p)

-------------------------------------------------------------------------------
-- Compute probability of win
-------------------------------------------------------------------------------

type ProbWin p = Val p Prob

type Adversaries p = PlayerState [(Strategy p, ProbWin p)]

probWinWith :: Ord p => Game p -> Player -> Strategy p ->
               ProbWin p -> Player -> p -> (Prob, ProbWin p)
probWinWith game wpl adv = Game.dfsWith pre post
  where
    pre pl p =
        case game pl p of
          Left e -> Left (boolProb (Game.winning wpl e))
          Right ps -> Right (map swap $ strategize pl ps)

    post _ _ [] = error "no moves"
    post pl _ wpps =
        if pl == wpl then maximum ps
        else expectation (normalize ws) ps
      where
        (wps,mps) = unzip wpps
        ws = map fst wps
        ps = map (fromMaybe 0.0) mps

    strategize pl = if pl == wpl then weightlessStrategy else applyStrategy adv

probWin :: Ord p => Game p -> Player -> Strategy p -> Player -> p -> ProbWin p
probWin game wpl adv pl p = snd $ probWinWith game wpl adv Map.empty pl p

moveDist :: Ord p => Game p -> Solve p ->
            Adversaries p -> Player -> p -> ([(Prob,p)], Adversaries p)
moveDist game sol advs pl p =
    case game pl p of
      Left _ -> ([],advs)
      Right ps -> Game.updatePlayerState (dist ps) advs wpl
  where
    dist ps adv = (zip (normalize ws) ps, adv')
      where
        (ws,adv') = weight (mfilter notBad ps) adv

    weight ps [] = (uniform ps, [])
    weight ps (ah : at) = (ws, reverse ral ++ adv' : al)
      where
        (ral,adv,al) = foldr strengthen ([],ah,at) ps
        (prs,adv') = mapLR mprob adv ps
        ws = if any likely prs then map square prs else uniform ps

    strengthen Nothing = id
    strengthen (Just q) = go
      where
        go (ral, ah, []) = (ral,ah,[])
        go (ral, ah, adv : at) =
            if likely pr then go (ah : ral, adv', at)
            else (ral, ah, adv' : at)
          where
            (pr,adv') = prob adv q

    mprob adv Nothing = (0.0,adv)
    mprob adv (Just q) = prob adv q

    prob (adv,pw) q = (pr,(adv,pw'))
      where (pr,pw') = probWinWith game wpl adv pw pl' q

    notBad = not . Game.betterResult pl ev . Game.evalUnsafe sol pl'

    ev = Game.evalUnsafe sol pl p

    wpl = if Game.winning pl ev then pl' else pl

    pl' = Game.turn pl

    mfilter = map . mpred

    mpred f x = if f x then Just x else Nothing

    uniform = map (boolProb . isJust)

    likely pr = 0.5 <= pr

    square x = x * x
