{- |
module: $Header$
description: Finite two-player games
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Solve.Game
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe,isJust,mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Solve.Graph as Graph
import Solve.Util

-------------------------------------------------------------------------------
-- Players
-------------------------------------------------------------------------------

--
-- Player1 moves first
--
data Player =
    Player1
  | Player2
  deriving (Eq,Ord,Show)

newtype PlayerState s = PlayerState (s,s)

turn :: Player -> Player
turn Player1 = Player2
turn Player2 = Player1

getPlayerState :: PlayerState s -> Player -> s
getPlayerState (PlayerState (s1,_)) Player1 = s1
getPlayerState (PlayerState (_,s2)) Player2 = s2

updatePlayerState :: (s -> (a,s)) -> PlayerState s -> Player -> (a, PlayerState s)
updatePlayerState f (PlayerState (s1,s2)) Player1 = (x, PlayerState (s1',s2))
  where (x,s1') = f s1
updatePlayerState f (PlayerState (s1,s2)) Player2 = (x, PlayerState (s1,s2'))
  where (x,s2') = f s2

-------------------------------------------------------------------------------
-- Position evaluations
-------------------------------------------------------------------------------

data Eval =
    Win Player Int
  | Draw
  deriving (Eq,Show)

--
-- Best result for Player1 compares highest
--
instance Ord Eval where
  compare (Win Player1 n1) (Win Player1 n2) = compare n2 n1
  compare (Win Player1 _) _ = GT
  compare _ (Win Player1 _) = LT
  compare (Win Player2 n1) (Win Player2 n2) = compare n1 n2
  compare (Win Player2 _) _ = LT
  compare _ (Win Player2 _) = GT
  compare Draw Draw = EQ

better :: Ord a => Player -> a -> a -> Bool
better Player1 x y = x > y
better Player2 x y = x < y

best :: Ord a => Player -> [a] -> a
best Player1 = maximum
best Player2 = minimum

betterEval :: Player -> Eval -> Eval -> Bool
betterEval pl (Win pl1 _) (Win pl2 _) = pl1 == pl && pl2 /= pl
betterEval pl (Win pl1 _) Draw = pl1 == pl
betterEval pl Draw (Win pl2 _) = pl2 /= pl
betterEval _ Draw Draw = False

sameEval :: Eval -> Eval -> Bool
sameEval e1 e2 = not (betterEval Player1 e1 e2 || betterEval Player2 e1 e2)

winning :: Player -> Eval -> Bool
winning pl e = betterEval pl e Draw

win :: Player -> Eval
win p = Win p 0

delay :: Eval -> Eval
delay (Win p n) = Win p (n + 1)
delay Draw = Draw

turnEval :: Eval -> Eval
turnEval (Win pl n) = Win (turn pl) n
turnEval Draw = Draw

-------------------------------------------------------------------------------
-- Game definition
-------------------------------------------------------------------------------

--
-- The list of legal moves must not be empty or contain duplicate positions
--
type Game p = Player -> p -> Either Eval [p]

move :: Game p -> Player -> p -> [p]
move game pl p =
    case game pl p of
      Left _ -> []
      Right ps -> ps

-------------------------------------------------------------------------------
-- Depth-first search
-------------------------------------------------------------------------------

type DfsPre p a v = Player -> Graph.DfsPre p a v

type DfsPost p a v = Player -> Graph.DfsPost p a v

type Val p v = Graph.DfsResult (Player,p) v

dfsWith :: Ord p => DfsPre p a v -> DfsPost p a v ->
           Val p v -> Player -> p -> (v, Val p v)
dfsWith pre post = curry . Graph.dfsWith pre' post'
  where
    pre' (pl,p) =
        case pre pl p of
          Left v -> Left v
          Right aps -> Right (map (addPl (turn pl)) aps)

    post' (pl,p) = post pl p . map delPl

    addPl pl (a,p) = (a,(pl,p))

    delPl ((a,(_,p)),v) = ((a,p),v)

eval :: Ord p => Val p v -> Player -> p -> Maybe v
eval = curry . Graph.eval

evalUnsafe :: Ord p => Val p v -> Player -> p -> v
evalUnsafe = curry . Graph.evalUnsafe

-------------------------------------------------------------------------------
-- Game solution
-------------------------------------------------------------------------------

type Solve p = Val p Eval

solveWith :: Ord p => Game p -> Solve p -> Player -> p -> (Eval, Solve p)
solveWith game = dfsWith pre post
  where
    pre pl p =
        case game pl p of
          Left v -> Left v
          Right ps -> Right (map ((,) ()) ps)

    post pl _ = delay . best pl . map (fromMaybe Draw . snd)

solve :: Ord p => Game p -> Player -> p -> Solve p
solve game pl p = snd $ solveWith game Map.empty pl p

reachable :: Solve p -> Int
reachable = Map.size

-------------------------------------------------------------------------------
-- Forcing states that satisfy a predicate
-------------------------------------------------------------------------------

data Force =
    ForceIn Int
  | ForceNever
  deriving (Eq,Ord,Show)

type Forced p = Val p Force

bestForce :: Ord a => Player -> Player -> [a] -> a
bestForce fpl pl = if fpl == pl then minimum else maximum

delayForce :: Force -> Force
delayForce (ForceIn n) = ForceIn (n + 1)
delayForce ForceNever = ForceNever

forcedWith :: Ord p => Game p -> Player -> (Player -> p -> Bool) ->
              Forced p -> Player -> p -> (Force, Forced p)
forcedWith game fpl isp = dfsWith pre post
  where
    pre pl p =
        case game pl p of
          Left _ -> Left (if isp pl p then ForceIn 0 else ForceNever)
          Right ps -> Right (map ((,) ()) ps)

    post pl p =
        if isp pl p then const (ForceIn 0)
        else delayForce . bestForce fpl pl . map (fromMaybe ForceNever . snd)

forced :: Ord p => Game p -> Player -> (Player -> p -> Bool) ->
          Player -> p -> Forced p
forced game fpl isp pl p = snd $ forcedWith game fpl isp Map.empty pl p

-------------------------------------------------------------------------------
-- Maximizing a position value over a game
-------------------------------------------------------------------------------

data Max v = Max v Int
  deriving (Show,Eq)

instance Ord v => Ord (Max v) where
  compare (Max v1 k1) (Max v2 k2) =
      case compare v1 v2 of
        LT -> LT
        EQ -> compare k2 k1
        GT -> GT

gameMaxWith :: (Ord p, Ord v) => Game p -> Player -> (Player -> p -> v) ->
               Val p (Max v) -> Player -> p -> (Max v, Val p (Max v))
gameMaxWith game mpl pv = dfsWith pre post
  where
    pre pl p =
        case game pl p of
          Left _ -> Left (valNow pl p)
          Right ps -> Right (map ((,) ()) ps)

    post pl p = optimize . map valLater . mapMaybe snd
      where
        optimize [] = vk
        optimize vks = max vk ((if pl == mpl then maximum else minimum) vks)
        vk = valNow pl p

    valNow pl p = Max (pv pl p) 0
    valLater (Max v k) = Max v (k + 1)

gameMax :: (Ord p, Ord v) => Game p -> Player -> (Player -> p -> v) ->
           Player -> p -> Val p (Max v)
gameMax game mpl pv pl p = snd $ gameMaxWith game mpl pv Map.empty pl p

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

sameEvalStrategy :: Eval -> (p -> Eval) -> Strategy p
sameEvalStrategy e pe = filterStrategy (sameEval e . pe)

maxStrategy :: Ord v => (p -> v) -> Strategy p
maxStrategy _ [] = []
maxStrategy pv ps = mapMaybe f $ zip ps vs
  where
    f (p,w) = if w == v then Just p else Nothing
    vs = map (pv . snd) ps
    v = maximum vs

bestStrategy :: Player -> (p -> Eval) -> Strategy p
bestStrategy Player1 pe = maxStrategy pe
bestStrategy Player2 pe = maxStrategy (turnEval . pe)

stopLossStrategy :: Ord p => Solve p -> Player -> Int -> Strategy p
stopLossStrategy sol pl n = filterStrategy f
  where
    f p = let e = evalUnsafe sol pl' p in not (better pl' e ok)
    ok = Win pl' n
    pl' = turn pl

forcedStrategy :: Ord p => Forced p -> Player -> Int -> Strategy p
forcedStrategy frc pl n = filterStrategy f
  where f = (>) (ForceIn n) . evalUnsafe frc (turn pl)

-------------------------------------------------------------------------------
-- Validating strategies
-------------------------------------------------------------------------------

type StrategyFail p = Set ((Eval,p),(Eval,p),(Eval,p))

validateStrategy :: Ord p => Game p -> Solve p -> Player -> Strategy p -> Player -> p -> StrategyFail p
validateStrategy game sol spl str = \ipl -> fst . dfsWith pre post Map.empty ipl
  where
    pre pl p =
        case game pl p of
          Left _ -> Left Set.empty
          Right ps -> Right (strategize pl ps)

    post pl p pfs =
        if pl /= spl then fs
        else if betterEval pl (fst z) (fst n) then fs'
        else fs
      where
        fs = Set.unions (mapMaybe snd pfs)
        fs' = Set.insert (evalSol pl p, n, z) fs
        z = bestStr (move game pl p)
        n = bestStr (map (snd . fst) pfs)

    strategize pl = if pl == spl then applyStrategy str else weightlessStrategy

    bestStr = best spl . map (evalSol (turn spl))

    evalSol pl p = (evalUnsafe sol pl p, p)

-------------------------------------------------------------------------------
-- Compute probability of win
-------------------------------------------------------------------------------

type ProbWin p = Val p Prob

type Adversaries p = PlayerState [(Strategy p, ProbWin p)]

probWinWith :: Ord p => Game p -> Player -> Strategy p ->
               ProbWin p -> Player -> p -> (Prob, ProbWin p)
probWinWith game wpl adv = dfsWith pre post
  where
    pre pl p =
        case game pl p of
          Left e -> Left (boolProb (winning wpl e))
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
      Right ps -> updatePlayerState (dist ps) advs wpl
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

    notBad = not . betterEval pl ev . evalUnsafe sol pl'

    ev = evalUnsafe sol pl p

    wpl = if winning pl ev then pl' else pl

    pl' = turn pl

    mfilter = map . mpred

    mpred f x = if f x then Just x else Nothing

    uniform = map (boolProb . isJust)

    likely pr = 0.5 <= pr

    square x = x * x
