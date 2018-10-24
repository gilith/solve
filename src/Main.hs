{- |
module: Main
description: Solving simple games
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module Main
--  ( main )
where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
--import qualified System.Environment as Environment
import System.FilePath ((</>),(<.>))
import System.IO (IOMode(..),hPutStrLn,withFile)

import qualified Solve.FoxHounds as FH
import Solve.Game (Eval(..),Event(..),Max(..),Moves,Player(..))
import qualified Solve.Game as Game
import qualified Solve.NoughtsCrosses as NC
import Solve.Strategy (Strategy,StrategyFail)
import qualified Solve.Strategy as Strategy
import Solve.Util

-------------------------------------------------------------------------------
-- Noughts & Crosses
-------------------------------------------------------------------------------

dimNC :: String
dimNC = let n = show NC.boardSize in n ++ "x" ++ n

reachableNC :: Int
reachableNC = Game.reachable NC.solution

-------------------------------------------------------------------------------
-- Fox & Hounds
-------------------------------------------------------------------------------

dimFH :: String
dimFH = let n = show FH.boardSize in n ++ "x" ++ n

dbFH :: FilePath
dbFH = "doc" </> ("fox-hounds-" ++ dimFH) <.> "sql"

reachableFH :: Int
reachableFH = Game.reachable FH.solution

solutionFH :: Eval
solutionFH = FH.evalInitial FH.solution

winnerFH :: Player
winnerFH =
    case solutionFH of
      Win pl _ -> pl
      _ -> error "no winner"

depthFH :: Moves
depthFH =
    case solutionFH of
      Win _ n -> n
      _ -> error "no winner"

reachableIdxFH :: (FH.Idx,FH.Idx)
reachableIdxFH = (a,b)
  where
    a = minimum l
    b = maximum l
    l = map (FH.posToIdx . snd . fst) $ Map.toList FH.solution

incorrectPositionEvaluationsFH :: [(Player,FH.Pos,Eval,Eval)]
incorrectPositionEvaluationsFH = mapMaybe diff $ Map.toList FH.solution
  where
    diff ((pl,p),ev) =
        if Game.sameResult ev ev' then Nothing else Just (pl,p,ev,ev')
      where
        ev' = Game.evalUnsafe solution pl p

    solution = Game.solve game Player1 FH.initial

    game pl p =
        if null ps then Left (Game.winEval (Game.turn pl))
        else Right ps
      where
        ps = FH.move pl p

showIncorrectPositionEvaluationsFH :: [(Player,FH.Pos,Eval,Eval)] -> String
showIncorrectPositionEvaluationsFH ds =
    show (length ds) ++ concatMap showDiff ds
  where
    showDiff (pl,p,e,e') =
        "\n\n" ++ FH.ppPlayer pl ++ " to move" ++ show p ++
        "Incorrect position evaluation: " ++ FH.ppEval e ++ "\n" ++
        "Correct position evaluation: " ++ FH.ppEval e'

foxBoxStrategyFailFH :: StrategyFail FH.Pos
foxBoxStrategyFailFH =
    FH.validateStrategy Player2
      (Strategy.tryStrategy (FH.foxBoxStrategy 1))

showStrategyFailFH :: StrategyFail FH.Pos -> String
showStrategyFailFH ps =
    show n ++ (if n == 0 then "" else "\n" ++ tableFails)
  where
    n = Set.size ps
    showPos (e,p) = tail (show p) ++ FH.ppEval e
    rowsFail (a,b,c) =
        [] : zipWith3 tripleton (linesPos a) (linesPos b) (linesPos c)
      where
        linesPos = lines . showPos
    header = ["Position","Strategy move","Better move"]
    tableFails =
        showTable ([] : header : concat (map rowsFail (Set.toList ps)) ++ [[]])

houndsStopLossFH :: Int -> Strategy FH.Pos
houndsStopLossFH n = Strategy.tryStrategy (FH.stopLossStrategy Player2 n)

houndsStopLossFoxBox1FH :: Int -> Strategy FH.Pos
houndsStopLossFoxBox1FH n =
    Strategy.thenStrategy
      (Strategy.tryStrategy (FH.stopLossStrategy Player2 n))
      (Strategy.tryStrategy (FH.foxBoxStrategy 1))

houndsVictoryFH :: (Player,FH.Pos)
houndsVictoryFH = FH.typical f
  where f pl p = Game.evalUnsafe FH.solution pl p == Win Player2 0

foxEscapedFH :: (Player,FH.Pos)
foxEscapedFH = FH.typical (const FH.foxEscaped)

foxVictoryWithoutEscapeFH :: (Player,FH.Pos)
foxVictoryWithoutEscapeFH = FH.typical f
  where f pl p = Game.evalUnsafe FH.solution pl p == Win Player1 0 &&
                 not (FH.foxEscaped p)

typicalFoxBoxFH :: (Player,FH.Pos)
typicalFoxBoxFH = FH.typical f
  where f pl p = pl == Player1 &&
                 FH.isFoxBox p &&
                 case Game.evalUnsafe FH.maxFoxBox pl p of
                   Max (In n) _ -> n > 0
                   Max Never _ -> True

getPositionFH :: String -> (Player,FH.Pos)
getPositionFH "initial" = (Player1,FH.initial)
getPositionFH "opposite" = FH.opposite
getPositionFH "hounds victory" = houndsVictoryFH
getPositionFH "fox escaped" = foxEscapedFH
getPositionFH "fox victory without escape" = foxVictoryWithoutEscapeFH
getPositionFH "typical FoxBox" = typicalFoxBoxFH
getPositionFH _ = error "unknown position"

initialPositionsFH :: (String,String)
initialPositionsFH =
    case winnerFH of
      Player1 -> ("opposite","initial")
      Player2 -> ("initial","opposite")

ppPositionFH :: String -> String
ppPositionFH s =
    sp ++ ": " ++ FH.ppPlayer pl ++ " to move" ++ show p ++
    sp ++ " evaluation: " ++ FH.ppEval ev ++ "\n" ++
    sp ++ " maximum FoxBox: " ++ show fb ++ "\n" ++
    sp ++ " index: " ++ show idx
  where
    ev = Game.evalUnsafe FH.solution pl p
    fb = Game.evalUnsafe FH.maxFoxBox pl p
    idx = FH.posToIdx p
    (pl,p) = getPositionFH s
    sp = ucfirst s ++ " position"

probWinFH :: Int -> ((Prob,Prob,Prob),Prob)
probWinFH n = ((f1,f2,f3),h1)
  where
    f1 = pfw $ houndsStopLossFH n
    f2 = pfw $ houndsStopLossFoxBox1FH n
    f3 = pfw $ FH.houndsStrategy n

    h1 = phw $ FH.foxStrategy n

    pfw = pw Player1 (getPositionFH (fst initialPositionsFH))
    phw = pw Player2 (getPositionFH (snd initialPositionsFH))

    pw spl (pl,p) adv = fst $ Strategy.probWinWith FH.game spl adv Map.empty pl p

probDepthFH :: [(Int,((Prob,Prob,Prob),Prob))]
probDepthFH = map f [0..depthFH]
  where f n = (n, probWinFH n)

showProbDepthFH :: [(Int,((Prob,Prob,Prob),Prob))] -> String
showProbDepthFH ps =
    showTable
      ([] :
       ["Strategy", "Fox", "Fox wins", "Fox wins", "Hounds"] :
       ["depth", "wins", "vs", "vs", "win"] :
       ["", "vs", "StopLoss", "StopLoss", "vs"] :
       ["", "StopLoss", "+FoxBox1", "+ FoxBox", "StopLoss"] :
       ["", "from", "from", "from", "from"] :
       ["", ifp, ifp, ifp, ihp] :
       [] :
       map row ps ++
       [[]])
  where
    (ifp,ihp) = initialPositionsFH

    row (n,((f1,f2,f3),h1)) =
        [show n, showProb f1, showProb f2, showProb f3, showProb h1]

maxIntCdfFH :: Integer
maxIntCdfFH = 100000

posEntryFH :: (Player,FH.Pos) ->
              (FH.Idx, (Bool,Int), Event, Max Event, [(FH.Idx,Integer)])
posEntryFH (pl,p) = (FH.posToIdx p, w, fb, fbm, moves mvs)
  where
    w = (FH.winningForFox pl p, FH.winDepth pl p)
    fb = Game.evalUnsafe FH.foxBox pl p
    fbm = Game.evalUnsafe FH.maxFoxBox pl p
    mvs = FH.moveDist pl p

    moves = fst . mapLR cdf 0.0 . map snd . List.sort . map posIdx
      where
        cdf s (q,pr) = let s' = s + pr in ((q, round (s' * m)), s')
        posIdx (pr,q) = (FH.coordToSquare (moved q), (FH.posToIdx q, pr))
        moved = Set.findMin . Set.difference (pieces p) . pieces
        pieces q = Set.insert (FH.fox q) (FH.hounds q)
        m = fromInteger maxIntCdfFH

posTableFH :: [(FH.Idx, (Bool,Int), Event, Max Event, [(FH.Idx,Integer)])]
posTableFH = map posEntryFH $ Map.keys FH.solution

showPosEntryFH :: (FH.Idx, (Bool,Int), Event, Max Event, [(FH.Idx,Integer)]) ->
                  String
showPosEntryFH (pos, (wf,wd), fb, Max fbv fbk, mvs) =
    "INSERT INTO `foxhounds` VALUES " ++
    "(" ++ List.intercalate "," values ++ ");"
  where
    values =
      show pos :
      showBool wf :
      show wd :
      showEvent fb :
      showEvent fbv :
      show fbk :
      show mvn :
      concat (map showMove mvs) ++
      replicate (3 * (8 - mvn)) "NULL"

    mvn = length mvs

    showEvent (In k) = show k
    showEvent Never = "NULL"

    showMove (pos',cdf) = [show pos', "0", show cdf]

    showBool True = "'T'"
    showBool False = "'F'"

writePosTableFH :: FilePath ->
                   [(FH.Idx, (Bool,Int), Event, Max Event, [(FH.Idx,Integer)])] -> IO ()
writePosTableFH file entries = withFile file WriteMode $ \h ->
    mapM_ (hPutStrLn h . showPosEntryFH) entries

-------------------------------------------------------------------------------
-- Top-level
-------------------------------------------------------------------------------

lineLength :: Int
lineLength = 75

main :: IO ()
main = do
    --args <- Environment.getArgs
    ___
    putStrLn "NOUGHTS & CROSSES"
    putStrLn ""
    putStrLn $ "Board size: " ++ dimNC
    putStrLn $ "Reachable positions: " ++ show reachableNC
    putStrLn $ "Possible games: " ++ ppGames NC.gamesInitial
    ___
    putStrLn "FOX & HOUNDS"
    putStrLn ""
    putStrLn $ "Board size: " ++ dimFH
    putStrLn $ "Reachable positions: " ++ show reachableFH
    putStrLn $ "Reachable position index range: " ++ show reachableIdxFH
    putStrLn $ "Possible games: " ++ ppGames FH.gamesInitial
    putStrLn ""
    putStrLn $ ppPositionFH "initial"
    putStrLn ""
    putStrLn $ ppPositionFH "opposite"
    putStrLn ""
    putStrLn $ ppPositionFH "hounds victory"
    putStrLn ""
    putStrLn $ ppPositionFH "fox escaped"
    putStrLn ""
    putStrLn $ ppPositionFH "fox victory without escape"
    putStrLn ""
    putStrLn $ ppPositionFH "typical FoxBox"
    putStrLn ""
    putStrLn $ "Incorrect position evaluations: " ++ showIncorrectPositionEvaluationsFH incorrectPositionEvaluationsFH
    putStrLn ""
    putStr $ "FoxBox strategy failure positions: " ++ showStrategyFailFH foxBoxStrategyFailFH
    putStrLn ""
    putStrLn $ "Win probabilities against strategies of different depths:"
    putStrLn $ showProbDepthFH probDepthFH
    putStr $ "Creating game database in " ++ dbFH ++ ":"
    writePosTableFH dbFH posTableFH
    putStrLn $ " " ++ show (length posTableFH) ++ " rows"
    ___
    return ()
  where
    ppGames n = let s = show n in s ++ " (~10^" ++ show (length s - 1) ++ ")"
    ___ = putStrLn $ replicate lineLength '_'
