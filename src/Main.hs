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

import qualified Solve.FoxHounds as FH
import Solve.Game (Eval(..),Player(..),Solve,Strategy,StrategyFail)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Fox & Hounds
-------------------------------------------------------------------------------

reachableFH :: Int
reachableFH = Game.reachable FH.solution

solutionFH :: Eval
solutionFH = Game.evalUnsafe FH.solution Player1 FH.initial

winnerFH :: Player
winnerFH =
    case solutionFH of
      Win pl _ -> pl
      _ -> error "no winner"

initialIdxFH :: FH.Idx
initialIdxFH = FH.posToIdx FH.initial

maxReachableIdxFH :: FH.Idx
maxReachableIdxFH =
    maximum $ map (FH.posToIdx . snd . fst) $
    Map.toList FH.solution

depthFH :: Int
depthFH =
    case solutionFH of
      Win _ n -> n
      _ -> error "no winner"

stopLossFH :: Player -> Int -> Strategy FH.Pos
stopLossFH pl n = Game.tryStrategy (FH.stopLossStrategy pl n)

stopLossFoxBoxFH :: Int -> Strategy FH.Pos
stopLossFoxBoxFH n =
    Game.thenStrategy
      (stopLossFH Player2 n)
      (Game.tryStrategy FH.foxBoxStrategy)

foxBoxStrategyFailFH :: StrategyFail FH.Pos
foxBoxStrategyFailFH =
    FH.validateStrategy Player2
      (Game.tryStrategy FH.foxBoxStrategy)

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
    (FH.probWin Player1 (stopLossFH Player2 n),
     FH.probWin Player1 (stopLossFoxBoxFH n),
     FH.probWin Player2 (stopLossFH Player1 n))

probDepthFH :: [(Int,Prob,Prob,Prob)]
probDepthFH = map f [0..depthFH]
  where
    f n = let (p1,p2,q) = probWinFH n in (n,p1,p2,q)

showProbDepthFH :: [(Int,Prob,Prob,Prob)] -> String
showProbDepthFH ps =
    showTable
      ([] :
       ["Stop-loss", "Fox wins", "Fox wins", "  Hounds"] :
       ["depth", "", "(FoxBox)", "win"] :
       [] :
       map row ps ++
       [[]])
  where
    row (n,f1,f2,h) = [show n, showProb f1, showProb f2, showProb h]

maxIntCdfFH :: Int
maxIntCdfFH = 100000

posEntryFH :: Solve FH.Pos -> Player -> FH.Pos -> (FH.Idx,Bool,[FH.Idx])
posEntryFH sol pl p = (FH.posToIdx p, fw, mvs)
  where
    fw = Game.winning Player1 (Game.evalUnsafe sol pl p)
    mvs = sortMoves $ Game.move FH.game pl p
    sortMoves = map snd . List.sort . map posIdx
    posIdx q = (FH.coordToSquare (moved q), FH.posToIdx q)
    moved = Set.findMin . Set.difference (pieces p) . pieces
    pieces q = Set.insert (FH.fox q) (FH.hounds q)

posTableFH :: Solve FH.Pos -> [(FH.Idx,Bool,[FH.Idx])]
posTableFH sol = map (uncurry (posEntryFH sol)) (Map.keys sol)

showPosEntryFH :: (FH.Idx,Bool,[FH.Idx]) -> String
showPosEntryFH (pos,fw,mvs) =
    "INSERT INTO `foxhounds` VALUES " ++
    "(" ++ List.intercalate "," values ++ ");\n"
  where
    values =
      show pos :
      showBool fw :
      show mvn :
      concat (map showMove mvs) ++
      replicate (3 * (FH.boardSize - mvn)) "NULL"

    mvn = length mvs

    showMove pos' = [show pos', "0", show maxIntCdfFH]

    showBool True = "'T'"
    showBool False = "'F'"

showPosTableFH :: [(FH.Idx,Bool,[FH.Idx])] -> String
showPosTableFH = concat . map showPosEntryFH;

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
    putStrLn $ "Maximum reachable position index: " ++ show maxReachableIdxFH
    putStrLn $ "Solution: " ++ FH.ppEval solutionFH ++ "\n"
    putStrLn $ "FoxBox strategy failure positions: " ++ showStrategyFailFH foxBoxStrategyFailFH
    putStrLn $ "Win probabilities against stop-loss strategies of different depths:"
    --putStrLn $ showProbDepthFH probDepthFH
    putStrLn $ "Creating game database in " ++ db
    writeFile db (showPosTableFH (posTableFH FH.solution))
    ___
    return ()
  where
    dim = let n = show FH.boardSize in n ++ "x" ++ n
    db = "doc" </> ("fox-hounds-" ++ dim) <.> "db"
    ___ = putStrLn $ replicate lineLength '_'
