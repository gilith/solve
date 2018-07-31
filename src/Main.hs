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

--import qualified System.Environment as Environment

import qualified Solve.FoxHounds as FoxHounds
import Solve.Game (Eval(..),Player(..))
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

fhProbWin :: Player -> Int -> Prob
fhProbWin pl n = FoxHounds.probWin pl adv
  where
    adv = Game.orelseAdversary
            (FoxHounds.stopLossAdversary pl n)
            Game.uniformAdversary

fhProbDepth :: [(Int,Prob,Prob)]
fhProbDepth = map f [0..fhDepth]
  where
    f n = (n, fhProbWin Player1 n, fhProbWin Player2 n)

showProbDepth :: [(Int,Prob,Prob)] -> String
showProbDepth ps =
    showTable (["Adversary", "Player1", "Player2"] : [] : map f ps)
  where
    f (n,p,q) = ["n = " ++ show n, showProb p, showProb q]

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
    putStrLn $ showProbDepth fhProbDepth
    ___
    return ()
  where
    ___ = putStrLn $ replicate lineLength '_'
