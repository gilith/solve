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

import Data.Function (on)
import Data.List (maximumBy)
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
-- Game length
-------------------------------------------------------------------------------

type Moves = Int

data Event =
    In Moves
  | Never
  deriving (Eq,Ord,Show)

now :: Event
now = In 0

delay :: Event -> Event
delay (In n) = In (n + 1)
delay Never = Never

nowOrNever :: Bool -> Event
nowOrNever True = now
nowOrNever False = Never

-------------------------------------------------------------------------------
-- Position evaluations
-------------------------------------------------------------------------------

data Eval =
    Win Player Moves
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

compareEval :: Player -> Eval -> Eval -> Ordering
compareEval Player1 = compare
compareEval Player2 = flip compare

betterEval :: Player -> Eval -> Eval -> Bool
betterEval pl x y =
    case compareEval pl x y of
      GT -> True
      _ -> False

bestEval :: Player -> [Eval] -> Eval
bestEval = maximumBy . compareEval

winEval :: Player -> Eval
winEval p = Win p 0

delayEval :: Eval -> Eval
delayEval (Win p n) = Win p (n + 1)
delayEval Draw = Draw

turnEval :: Eval -> Eval
turnEval (Win pl n) = Win (turn pl) n
turnEval Draw = Draw

betterResult :: Player -> Eval -> Eval -> Bool
betterResult pl (Win pl1 _) (Win pl2 _) = pl1 == pl && pl2 /= pl
betterResult pl (Win pl1 _) Draw = pl1 == pl
betterResult pl Draw (Win pl2 _) = pl2 /= pl
betterResult _ Draw Draw = False

sameResult :: Eval -> Eval -> Bool
sameResult e1 e2 =
    not (betterResult Player1 e1 e2 || betterResult Player2 e1 e2)

winning :: Player -> Eval -> Bool
winning pl e = betterResult pl e Draw

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
-- Breadth-first search
-------------------------------------------------------------------------------

bfs :: Ord p => Game p -> Player -> p -> [(Player,p)]
bfs game = curry $ Graph.bfs next
  where next (pl,p) = map ((,) (turn pl)) $ move game pl p

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

    post pl _ = delayEval . bestEval pl . map (fromMaybe Draw . snd)

solve :: Ord p => Game p -> Player -> p -> Solve p
solve game pl p = snd $ solveWith game Map.empty pl p

reachable :: Solve p -> Int
reachable = Map.size

-------------------------------------------------------------------------------
-- Forcing positions that satisfy a predicate
-------------------------------------------------------------------------------

type Force p = Val p Event

forceWith :: Ord p => Game p -> Player -> (Player -> p -> Bool) ->
             Force p -> Player -> p -> (Event, Force p)
forceWith game fpl isp = dfsWith pre post
  where
    best pl = if pl == fpl then minimum else maximum

    pre pl p =
        case game pl p of
          Left _ -> Left (nowOrNever (isp pl p))
          Right ps -> Right (map ((,) ()) ps)

    post pl p =
        if isp pl p then const now
        else delay . best pl . map (fromMaybe Never . snd)

force :: Ord p => Game p -> Player -> (Player -> p -> Bool) ->
         Player -> p -> Force p
force game fpl isp pl p = snd $ forceWith game fpl isp Map.empty pl p

-------------------------------------------------------------------------------
-- Maximizing a position value over a game
-------------------------------------------------------------------------------

data Max v = Max v Moves
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

sameResultStrategy :: Eval -> (p -> Eval) -> Strategy p
sameResultStrategy e pe = filterStrategy (sameResult e . pe)

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

stopLossStrategy :: Ord p => Solve p -> Player -> Moves -> Strategy p
stopLossStrategy sol pl n = filterStrategy f
  where
    f p = let e = evalUnsafe sol pl' p in not (betterEval pl' e ok)
    ok = Win pl' n
    pl' = turn pl

forceStrategy :: Ord p => Force p -> Player -> Moves -> Strategy p
forceStrategy frc pl n = filterStrategy f
  where f = (>) (In n) . evalUnsafe frc (turn pl)

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
        else if betterResult pl (fst z) (fst n) then fs'
        else fs
      where
        fs = Set.unions (mapMaybe snd pfs)
        fs' = Set.insert (evalSol pl p, n, z) fs
        z = bestStr (move game pl p)
        n = bestStr (map (snd . fst) pfs)

    strategize pl = if pl == spl then applyStrategy str else weightlessStrategy

    bestStr = maximumBy (compareEval spl `on` fst) . map (evalSol (turn spl))

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

    notBad = not . betterResult pl ev . evalUnsafe sol pl'

    ev = evalUnsafe sol pl p

    wpl = if winning pl ev then pl' else pl

    pl' = turn pl

    mfilter = map . mpred

    mpred f x = if f x then Just x else Nothing

    uniform = map (boolProb . isJust)

    likely pr = 0.5 <= pr

    square x = x * x
