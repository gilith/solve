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
import Data.List (intersperse,maximumBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe,mapMaybe)

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
  deriving (Eq,Ord,Show,Enum,Bounded)

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

gameOver :: Game p -> Player -> p -> Bool
gameOver game pl p =
    case game pl p of
      Left _ -> True
      Right _ -> False

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

perfectPlay :: Ord p => Game p -> Solve p -> Player -> p -> [(Player,p)]
perfectPlay game soln = go
  where
    go pl p = (pl,p) : (case game pl p of Left _ -> [] ; Right ps -> sel pl ps)
    sel pl ps = go pl' (fst $ maximumBy (compareEval pl `on` snd) (map f ps))
      where
        f p = (p, evalUnsafe soln pl' p)
        pl' = turn pl

-------------------------------------------------------------------------------
-- The number of possible games
-------------------------------------------------------------------------------

type Games p = Val p Integer

gamesWith :: Ord p => Game p -> Games p -> Player -> p -> (Integer, Games p)
gamesWith game = dfsWith pre post
  where
    pre pl p =
        case game pl p of
          Left _ -> Left 1
          Right ps -> Right (map ((,) ()) ps)

    post _ _ = sum . map (acyclic . snd)

    acyclic (Just n) = n
    acyclic Nothing = error "loopy game"

games :: Ord p => Game p -> Player -> p -> Games p
games game pl p = snd $ gamesWith game Map.empty pl p

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
-- Finding studies (sequences of only moves to win the game)
-------------------------------------------------------------------------------

type Study p = (Player, Val p Int)

studyWith :: Ord p => Game p -> Solve p ->
             Study p -> Player -> p -> (Int, Study p)
studyWith game soln (spl,sval0) pl0 p0 = (v1, (spl,sval1))
  where
    (v1,sval1) = dfsWith pre post sval0 pl0 p0

    pre pl p =
        case game pl p of
          Left e -> Left (if winning spl e then 1 else 0)
          Right ps -> Right (map ((,) ()) ps)

    post pl p =
        if not (winning spl e) then const 0
        else if pl == spl then uniq . mapMaybe (incWin . snd)
        else maximum . mapMaybe snd
      where
        incWin Nothing = Nothing
        incWin (Just v) = if v == 0 then Nothing else Just (v + 1)
        uniq [v] = v
        uniq _ = 1
        e = evalUnsafe soln pl p

study :: Ord p => Game p -> Solve p -> Player -> Player -> p -> Study p
study game soln spl pl p = snd $ studyWith game soln (spl,Map.empty) pl p

bestStudies :: Study p -> [(Player,p)]
bestStudies (spl,sval) = snd $ Map.foldrWithKey best (2,[]) sval
  where
    best (pl,_) _ bps | pl /= spl = bps
    best p v (b,ps) | otherwise =
        case compare v b of
          LT -> (b,ps)
          EQ -> (b, p : ps)
          GT -> (v, [p])

criticalPath :: Ord p => Game p -> Study p -> Player -> p -> [(Player,p)]
criticalPath game (_,sval) = go
  where
    go pl p = (pl,p) : (case game pl p of Left _ -> [] ; Right ps -> sel pl ps)
    sel pl ps = if v <= 1 then [(pl',p')] else go pl' p'
      where
        (p',v) = maximumBy (compare `on` snd) (map pv ps)
        pv p = (p, evalUnsafe sval pl' p)
        pl' = turn pl

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

class Printable p where
  ppPosition :: p -> String

  ppPlayer :: p -> Player -> String
  ppPlayer _ pl = show pl

  ppPlayerPosition :: Player -> p -> String
  ppPlayerPosition pl p = ppPlayer p pl ++ " to move\n" ++ ppPosition p

  ppEval :: p -> Eval -> String
  ppEval p (Win pl n) = ppPlayer p pl ++ " win in " ++ show n
  ppEval _ Draw = "Draw"

  ppPlay :: [(Player,p)] -> String
  ppPlay [] = ""
  ppPlay ((pl,p) : ps) = fmtTable fmt (intersperse [] ms)
    where
      ms = groupl 2 (if pl == Player1 then sl else "" : sl)
      sl = map ppPosition (p : map snd ps)
      fmt = Table {borderTable = False, alignLeftTable = True, paddingTable = 3}
