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

import qualified Data.Set as Set
--import qualified System.Environment as Environment

import qualified Solve.FoxHounds as FoxHounds
import Solve.Game (Eval(..),Player(..),Strategy,StrategyFail)
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Fox & hounds
-------------------------------------------------------------------------------

fhReachable :: Int
fhReachable = Game.reachable FoxHounds.solution

fhSolution :: Eval
fhSolution = Game.evalUnsafe FoxHounds.solution Player1 FoxHounds.initial

fhWinner :: Player
fhWinner = case fhSolution of
             Win pl _ -> pl
             _ -> error "no winner"

fhDepth :: Int
fhDepth = case fhSolution of
             Win _ n -> n
             _ -> error "no winner"

fhStopLoss :: Player -> Int -> Strategy FoxHounds.Pos
fhStopLoss pl n = Game.tryStrategy (FoxHounds.stopLossStrategy pl n)

fhStopLossFoxBox :: Int -> Strategy FoxHounds.Pos
fhStopLossFoxBox n =
    Game.thenStrategy
      (fhStopLoss Player2 n)
      (Game.tryStrategy FoxHounds.foxBoxStrategy)

fhFoxBoxStrategyFail :: StrategyFail FoxHounds.Pos
fhFoxBoxStrategyFail =
    FoxHounds.validateStrategy Player2
      (Game.tryStrategy FoxHounds.foxBoxStrategy)

fhShowStrategyFail :: StrategyFail FoxHounds.Pos -> String
fhShowStrategyFail ps =
    show n ++ (if n == 0 then "" else "\n" ++ tableFails)
  where
    n = Set.size ps
    showPos (e,p) = tail (show p) ++ FoxHounds.ppEval e
    rowsFail (a,b,c) =
        [] : zipWith3 tripleton (linesPos a) (linesPos b) (linesPos c)
      where
        linesPos = lines . showPos
    header = ["Position","Strategy move","Better move"]
    tableFails =
        showTable ([] : header : concat (map rowsFail (Set.toList ps)) ++ [[]])

fhProbWin :: Int -> (Prob,Prob,Prob)
fhProbWin n =
    (FoxHounds.probWin Player1 (fhStopLoss Player2 n),
     FoxHounds.probWin Player1 (fhStopLossFoxBox n),
     FoxHounds.probWin Player2 (fhStopLoss Player1 n))

fhProbDepth :: [(Int,Prob,Prob,Prob)]
fhProbDepth = map f [0..fhDepth]
  where
    f n = let (p1,p2,q) = fhProbWin n in (n,p1,p2,q)

fhShowProbDepth :: [(Int,Prob,Prob,Prob)] -> String
fhShowProbDepth ps =
    showTable
      ([] :
       ["Stop-loss", "Fox wins", "Fox wins", "  Hounds"] :
       ["depth", "", "(FoxBox)", "win"] :
       [] :
       map row ps ++
       [[]])
  where
    row (n,f1,f2,h) = [show n, showProb f1, showProb f2, showProb h]

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
    putStrLn $ "Board size: " ++ show FoxHounds.boardSize
    putStrLn $ "Reachable positions: " ++ show fhReachable
    putStrLn $ "Solution: " ++ FoxHounds.ppEval fhSolution ++ "\n"
    putStrLn $ "FoxBox strategy failure positions: " ++ fhShowStrategyFail fhFoxBoxStrategyFail
    putStrLn $ "Win probabilities against stop-loss strategies of various depths:"
    putStrLn $ fhShowProbDepth fhProbDepth
    ___
    return ()
  where
    ___ = putStrLn $ replicate lineLength '_'
