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

import Solve.Game (Eval(..),Event(..),Force,Game,Moves,Player(..),PlayerState(..),Solve,Val)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Strategies
-------------------------------------------------------------------------------

-- Weights are positive
type Weight = Double

-- Strategies may filter out positions and change weights
type Strategy p = [(Weight,p)] -> [(Weight,p)]

moveDistStrategy :: Eq p => Game p -> Strategy p -> Player -> p -> [(Prob,p)]
moveDistStrategy game str pl p =
    case game pl p of
      Left _ -> []
      Right ps -> distStrategy str ps

distStrategy :: Eq p => Strategy p -> [p] -> [(Prob,p)]
distStrategy str ps = map pdf ps
  where
    pdf p = case filter ((== p) . snd) pps of
              [] -> (0.0,p)
              pp : _ -> pp
    (ws,ps') = unzip $ applyStrategy str ps
    pps = zip (normalize ws) ps'

applyStrategy :: Strategy p -> [p] -> [(Weight,p)]
applyStrategy str ps =
    case str $ map ((,) 1.0) ps of
      [] -> error "strategy pruned away all moves"
      wps -> wps

weightlessStrategy :: [p] -> [(Weight,p)]
weightlessStrategy = map ((,) undefined)

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
orelseStrategy str1 str2 wps =
    case str1 wps of
      [] -> str2 wps
      wps' -> wps'

tryStrategy :: Strategy p -> Strategy p
tryStrategy = flip orelseStrategy idStrategy

filterStrategy :: (p -> Bool) -> Strategy p
filterStrategy f = filter (f . snd)

sameResultStrategy :: Eval -> (p -> Eval) -> Strategy p
sameResultStrategy e pe = filterStrategy (Game.sameResult e . pe)

maxStrategy :: Ord v => (p -> v) -> Strategy p
maxStrategy _ [] = []
maxStrategy pv ps = mapMaybe f $ zip ps vs
  where
    f (p,w) = if w == v then Just p else Nothing
    vs = map (pv . snd) ps
    v = maximum vs

bestStrategy :: Player -> (p -> Eval) -> Strategy p
bestStrategy Player1 pe = maxStrategy pe
bestStrategy Player2 pe = maxStrategy (Game.turnEval . pe)

stopLossStrategy :: Ord p => Solve p -> Player -> Moves -> Strategy p
stopLossStrategy sol pl n = filterStrategy f
  where
    f p = let e = Game.evalUnsafe sol pl' p in not (Game.betterEval pl' e ok)
    ok = Win pl' n
    pl' = Game.turn pl

forceStrategy :: Ord p => Force p -> Player -> Moves -> Strategy p
forceStrategy frc pl n = filterStrategy f
  where f = (>) (In n) . Game.evalUnsafe frc (Game.turn pl)

-------------------------------------------------------------------------------
-- Validating strategies
-------------------------------------------------------------------------------

type StrategyFail p = Set ((Eval,p),(Eval,p),(Eval,p))

validateStrategy :: Ord p => Game p -> Solve p -> Player -> Strategy p -> Player -> p -> StrategyFail p
validateStrategy game sol spl str = \ipl ->
    fst . Game.dfsWith pre post Map.empty ipl
  where
    pre pl p =
        case game pl p of
          Left _ -> Left Set.empty
          Right ps -> Right (strategize pl ps)

    post pl p pfs =
        if pl /= spl then fs
        else if Game.betterResult pl (fst z) (fst n) then fs'
        else fs
      where
        fs = Set.unions (mapMaybe snd pfs)
        fs' = Set.insert (evalSol pl p, n, z) fs
        z = bestStr (Game.move game pl p)
        n = bestStr (map (snd . fst) pfs)

    strategize pl = if pl == spl then applyStrategy str else weightlessStrategy

    bestStr = maximumBy (Game.compareEval spl `on` fst) .
              map (evalSol (Game.turn spl))

    evalSol pl p = (Game.evalUnsafe sol pl p, p)

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
          Right ps -> Right (strategize pl ps)

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
