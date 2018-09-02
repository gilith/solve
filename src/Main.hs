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
--import qualified System.Environment as Environment
import System.FilePath ((</>),(<.>))
import System.IO (IOMode(..),hPutStrLn,withFile)

import qualified Solve.FoxHounds as FH
import Solve.Game (Adversaries,Eval(..),Force(..),Player(..),Strategy,StrategyFail)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Fox & Hounds
-------------------------------------------------------------------------------

reachableFH :: Int
reachableFH = Game.reachable FH.solution

solutionFH :: Eval
solutionFH = FH.evalInitial FH.solution

winnerFH :: Player
winnerFH =
    case solutionFH of
      Win pl _ -> pl
      _ -> error "no winner"

initialIdxFH :: FH.Idx
initialIdxFH = FH.posToIdx FH.initial

reachableIdxFH :: (FH.Idx,FH.Idx)
reachableIdxFH = (a,b)
  where
    a = minimum l
    b = maximum l
    l = map (FH.posToIdx . snd . fst) $ Map.toList FH.solution

depthFH :: Int
depthFH =
    case solutionFH of
      Win _ n -> n
      _ -> error "no winner"

maxForcedFoxBoxFH :: (Game.Force,Int)
maxForcedFoxBoxFH = FH.evalInitial FH.maxForcedFoxBox

stopLossFH :: Player -> Int -> Strategy FH.Pos
stopLossFH pl n = Game.tryStrategy (FH.stopLossStrategy pl n)

foxBoxStrategyFailFH :: StrategyFail FH.Pos
foxBoxStrategyFailFH =
    FH.validateStrategy Player2
      (Game.tryStrategy (FH.foxBoxStrategy 1))

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

probWinFH :: Int -> (Prob,Prob,Prob)
probWinFH n =
    (FH.evalInitial $ FH.probWin Player1 (stopLossFH Player2 n),
     FH.evalInitial $ FH.probWin Player1 (FH.houndsStrategy n),
     FH.evalInitial $ FH.probWin Player2 (FH.foxStrategy n))

probDepthFH :: [(Int,Prob,Prob,Prob)]
probDepthFH = map f [0..depthFH]
  where
    f n = let (p1,p2,q) = probWinFH n in (n,p1,p2,q)

showProbDepthFH :: [(Int,Prob,Prob,Prob)] -> String
showProbDepthFH ps =
    showTable
      ([] :
       ["Strategy", "Fox wins", "Fox wins", "Hounds"] :
       ["depth", "vs", "vs", "win"] :
       ["", "StopLoss", "StopLoss", "vs"] :
       ["", "", "+ FoxBox", "StopLoss"] :
       [] :
       map row ps ++
       [[]])
  where
    row (n,f1,f2,h) = [show n, showProb f1, showProb f2, showProb h]

maxIntCdfFH :: Integer
maxIntCdfFH = 100000

posEntryFH :: Adversaries FH.Pos -> (Player,FH.Pos) ->
              ((FH.Idx,Bool,Force,(Force,Int),[(FH.Idx,Integer)]), Adversaries FH.Pos)
posEntryFH adv (pl,p) = ((FH.posToIdx p, fw, fb, fbm, moves mvs), adv')
  where
    fw = Game.winning Player1 (Game.evalUnsafe FH.solution pl p)
    fb = Game.evalUnsafe FH.forcedFoxBox pl p
    fbm = Game.evalUnsafe FH.maxForcedFoxBox pl p
    (mvs,adv') = FH.moveDist adv pl p

    moves = fst . mapLR cdf 0.0 . map snd . List.sort . map posIdx
      where
        cdf s (q,pr) = let s' = s + pr in ((q, round (s' * m)), s')
        posIdx (pr,q) = (FH.coordToSquare (moved q), (FH.posToIdx q, pr))
        moved = Set.findMin . Set.difference (pieces p) . pieces
        pieces q = Set.insert (FH.fox q) (FH.hounds q)
        m = fromInteger maxIntCdfFH

posTableFH :: [(FH.Idx,Bool,Force,(Force,Int),[(FH.Idx,Integer)])]
posTableFH = fst $ mapLR posEntryFH FH.adversaries $ Map.keys FH.solution

showPosEntryFH :: (FH.Idx,Bool,Force,(Force,Int),[(FH.Idx,Integer)]) -> String
showPosEntryFH (pos,fw,fb,(fbv,fbk),mvs) =
    "INSERT INTO `foxhounds` VALUES " ++
    "(" ++ List.intercalate "," values ++ ");"
  where
    values =
      show pos :
      showBool fw :
      showForce fb :
      showForce fbv :
      show fbk :
      show mvn :
      concat (map showMove mvs) ++
      replicate (3 * (8 - mvn)) "NULL"

    mvn = length mvs

    showForce (ForceIn k) = show k
    showForce ForceNever = "NULL"

    showMove (pos',cdf) = [show pos', "0", show cdf]

    showBool True = "'T'"
    showBool False = "'F'"

writePosTableFH :: FilePath -> [(FH.Idx,Bool,Force,(Force,Int),[(FH.Idx,Integer)])] -> IO ()
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
    putStrLn "FOX & HOUNDS"
    putStrLn ""
    putStrLn $ "Board size: " ++ dim
    putStrLn $ "Reachable positions: " ++ show reachableFH
    putStrLn $ "Initial position index: " ++ show initialIdxFH
    putStrLn $ "Reachable position index range: " ++ show reachableIdxFH
    putStrLn $ "Solution: " ++ FH.ppEval solutionFH
    putStrLn $ "Maximum forced FoxBox: " ++ show maxForcedFoxBoxFH
    putStrLn ""
    putStrLn $ "Win probabilities against strategies of different depths:"
    putStrLn $ showProbDepthFH probDepthFH
    putStr $ "Creating game database in " ++ db ++ ":"
    writePosTableFH db posTableFH
    putStrLn $ " " ++ show (length posTableFH) ++ " rows"
    ___
    return ()
  where
    dim = let n = show FH.boardSize in n ++ "x" ++ n
    db = "doc" </> ("fox-hounds-" ++ dim) <.> "sql"
    ___ = putStrLn $ replicate lineLength '_'
