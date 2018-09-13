{- |
module: Main
description: Testing simple games
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import Test.QuickCheck

import Solve.Game (Eval(..),Player(..))
import qualified Solve.Game as Game

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

checkWith :: Testable prop => Args -> (String,prop) -> IO ()
checkWith args (desc,prop) = do
    putStr (desc ++ " ")
    res <- quickCheckWithResult args prop
    case res of
      Failure {} -> error "Proposition failed"
      _ -> return ()

test :: Testable prop => (String,prop) -> IO ()
test = checkWith $ stdArgs {maxSuccess = 1000}

{- No assertions yet
assert :: (String,Bool) -> IO ()
assert = checkWith $ stdArgs {maxSuccess = 1}
-}

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Eval where
  arbitrary = do
      pl <- arbitrary
      n <- arbitrarySizedNatural
      return (if n == 0 then Draw else Win pl (n - 1))

-------------------------------------------------------------------------------
-- Testing position evaluations
-------------------------------------------------------------------------------

checkBetterResult :: Player -> Eval -> Eval -> Bool
checkBetterResult pl e1 e2 =
    not (Game.betterResult pl e1 e2) || Game.betterEval pl e1 e2

testEval :: IO ()
testEval = do
    test ("betterResult agrees with betterEval",checkBetterResult)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main :: IO ()
main = do
    testEval
