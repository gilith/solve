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

import Data.Set (Set)
--import qualified System.Environment as Environment

import qualified Solve.FoxHounds as FoxHounds
import Solve.Game (Adversary,Eval(..),Player(..))
import qualified Solve.Game as Game
import Solve.Util

-------------------------------------------------------------------------------
-- Fox & hounds
-------------------------------------------------------------------------------

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

fhStopLoss :: Player -> Int -> Adversary FoxHounds.Pos
fhStopLoss pl n =
    Game.orelseAdversary
      (FoxHounds.stopLossAdversary pl n)
      Game.uniformAdversary

fhFoxBox :: Int -> Adversary FoxHounds.Pos
fhFoxBox n =
    Game.orelseAdversary
      FoxHounds.foxBoxAdversary
      (fhStopLoss Player1 n)

fhValidateFoxBox :: Set (FoxHounds.Pos,(Eval,FoxHounds.Pos),(Eval,FoxHounds.Pos))
fhValidateFoxBox = Game.validateAdversary FoxHounds.game FoxHounds.solution Player1 adv Player1 FoxHounds.initial
  where
    adv =Game.orelseAdversary FoxHounds.foxBoxAdversary Game.uniformAdversary

fhProbWin :: Int -> (Prob,Prob,Prob)
fhProbWin n =
    (FoxHounds.probWin Player1 (fhStopLoss Player1 n),
     FoxHounds.probWin Player1 (fhFoxBox n),
     FoxHounds.probWin Player2 (fhStopLoss Player2 n))

fhProbDepth :: [(Int,Prob,Prob,Prob)]
fhProbDepth = map f [0..fhDepth]
  where
    f n = let (p1,p2,q) = fhProbWin n in (n,p1,p2,q)

showProbDepth :: [(Int,Prob,Prob,Prob)] -> String
showProbDepth ps =
    showTable (["Stop loss", "Fox win", "Fox win*", "Hounds win"] :
               [] :
               map f ps)
  where
    f (n,p1,p2,q) = ["depth " ++ show n, showProb p1, showProb p2, showProb q]

-------------------------------------------------------------------------------
-- Top-level
-------------------------------------------------------------------------------

lineLength :: Int
lineLength = 75

main :: IO ()
main = do
    --args <- Environment.getArgs
    ___
    putStrLn "Fox & Hounds:"
    putStrLn ""
    putStrLn $ "Board size = " ++ show FoxHounds.boardSize
    putStrLn $ "Solution = " ++ show (fhSolution)
    putStrLn ""
    putStrLn $ "Bad fox-box strategy = " ++ show fhValidateFoxBox
    putStrLn ""
    putStrLn $ showProbDepth fhProbDepth
    ___
    return ()
  where
    ___ = putStrLn $ replicate lineLength '_'
