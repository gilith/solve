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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Numeric (showFFloat)
import qualified System.Environment as Environment
import System.FilePath ((</>),(<.>))
import System.IO (IOMode(..),hPutStrLn,withFile)

import qualified Solve.FoxHounds as FH
import Solve.Game (Eval(..),Event(..),Max(..),Moves,Player(..))
import qualified Solve.Game as Game
import qualified Solve.NoughtsCrosses as NC
import qualified Solve.QueenPawns as QP
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

ppBestStudyNC :: Player -> String
ppBestStudyNC pl =
    case Game.bestStudies stdy of
      [] -> "No good " ++ spl ++ " studies"
      ((_,p) : _) -> spl ++ " to move and win:\n\n" ++
                     Game.ppPlay (Game.criticalPath NC.game stdy pl p)
  where
    stdy = NC.study pl
    spl = NC.ppPlayer pl

analyzeNC :: IO ()
analyzeNC = do
    putStrLn $ "Board size: " ++ dimNC
    putStrLn $ "Reachable positions: " ++ ppInteger reachableNC
    putStrLn $ "Possible games: " ++ ppHugeInteger NC.gamesInitial
    putStrLn ""
    putStrLn $ "Perfect game (" ++ NC.ppEval (NC.evalInitial NC.solution) ++ "):\n"
    putStr $ Game.ppPlay (NC.perfectPlay Player1 NC.initial)
    putStrLn ""
    putStr $ ppBestStudyNC Player1
    putStrLn ""
    putStr $ ppBestStudyNC Player2

-------------------------------------------------------------------------------
-- Fox & Hounds
-------------------------------------------------------------------------------

type RowFH = (FH.Idx, (Bool,Int), Event, Max Event, [(FH.Idx,Integer)])

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
        "\n\n" ++ Game.ppPlayerPosition pl p ++
        "Incorrect position evaluation: " ++ FH.ppEval e ++ "\n" ++
        "Correct position evaluation: " ++ FH.ppEval e'

infiniteMaxFoxBoxFH :: [(Player,FH.Pos)]
infiniteMaxFoxBoxFH =
    map fst $ filter f $ Map.toList FH.maxFoxBox
  where
    f ((pl,p), Max n _) = (n == Never) /= FH.winningForFox pl p

showInfiniteMaxFoxBoxFH :: [(Player,FH.Pos)] -> String
showInfiniteMaxFoxBoxFH ps =
    show (length ps) ++ concatMap showPos ps
  where
    showPos (pl,p) =
        "\n\n" ++ Game.ppPlayerPosition pl p ++
        "Position evaluation: " ++ FH.ppEval ev ++ "\n" ++
        "Maximum FoxBox: " ++ show fb
      where
        ev = Game.evalUnsafe FH.solution pl p
        fb = Game.evalUnsafe FH.maxFoxBox pl p

foxBoxStrategyFailFH :: StrategyFail FH.Pos
foxBoxStrategyFailFH =
    FH.validateStrategy Player2
      (Strategy.tryStrategy (FH.foxBoxStrategy 1))

showStrategyFailFH :: StrategyFail FH.Pos -> String
showStrategyFailFH ps =
    show n ++ (if n == 0 then "" else "\n" ++ tableFails)
  where
    n = Set.size ps
    showPos (p,e) = tail (show p) ++ FH.ppEval e
    rowsFail (a,b,c) =
        [] : zipWith3 tripleton (linesPos a) (linesPos b) (linesPos c)
      where
        linesPos = lines . showPos
    header = ["Position","Strategy move","Better move"]
    tableFails =
        ppTable ([] : header : concat (map rowsFail (Set.toList ps)) ++ [[]])

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
    sp ++ ":\n" ++
    FH.ppPlayer pl ++ " to move" ++
    show p ++
    "Evaluation: " ++ FH.ppEval ev ++ "\n" ++
    "FoxBox: " ++ show fb ++ "\n" ++
    "Maximum FoxBox: " ++ show mfb ++ "\n" ++
    "Index: " ++ show idx
  where
    sp = ucfirst s ++ " position"
    ev = Game.evalUnsafe FH.solution pl p
    fb = Game.evalUnsafe FH.foxBox pl p
    mfb = Game.evalUnsafe FH.maxFoxBox pl p
    idx = FH.posToIdx p
    (pl,p) = getPositionFH s

ppBestStudyFH :: Player -> String
ppBestStudyFH pl =
    case Game.bestStudies stdy of
      [] -> "No good " ++ spl ++ " studies"
      ((_,p) : _) -> spl ++ " to move and win:\n" ++
                     Game.ppPlay (Game.criticalPath FH.game stdy pl p) ++
                     "Study position index: " ++ show (FH.posToIdx p) ++ "\n"
  where
    stdy = FH.study pl
    spl = FH.ppPlayer pl

probWinFH :: Int -> ((Prob,Prob,Prob),Prob)
probWinFH n = ((f1,f2,f3),h1)
  where
    f1 = pfw $ houndsStopLossFH n
    f2 = pfw $ houndsStopLossFoxBox1FH n
    f3 = pfw $ FH.houndsStrategyN n

    h1 = phw $ FH.foxStrategyN n

    pfw = pw Player1 (getPositionFH (fst initialPositionsFH))
    phw = pw Player2 (getPositionFH (snd initialPositionsFH))

    pw spl (pl,p) adv = fst $ Strategy.probWinWith FH.game spl adv Map.empty pl p

probDepthFH :: [(Int,((Prob,Prob,Prob),Prob))]
probDepthFH = map f [0..depthFH]
  where f n = (n, probWinFH n)

showProbDepthFH :: [(Int,((Prob,Prob,Prob),Prob))] -> String
showProbDepthFH ps =
    ppTable
      ([] :
       ["Strategy\ndepth",
        "Fox wins\nvs\nStopLoss\nfrom\n" ++ ifp ++ "\nposition",
        "Fox wins\nvs\nStopLoss\n+FoxBox1\nfrom\n" ++ ifp ++ "\nposition",
        "Fox wins\nvs\nStopLoss\n+ FoxBox\nfrom\n" ++ ifp ++ "\nposition",
        "Hounds\nwin\nvs\nStopLoss\nfrom\n" ++ ihp ++ "\nposition"] :
       [] :
       map row ps ++
       [[]])
  where
    (ifp,ihp) = initialPositionsFH

    row (n,((f1,f2,f3),h1)) =
        [show n, showProb f1, showProb f2, showProb f3, showProb h1]

fuzzTableFH :: (Prob,[(Prob,(Prob,Prob))])
fuzzTableFH = binary [] (0.0, entry 0.0) (1.0, entry 1.0) []
  where
    threshold = 0.00001
    target = 0.5

    binary ls (lf,lp) (uf,up) us =
        if uf - lf < threshold then
          (lf, reverse ls ++ [(lf,lp),(uf,up)] ++ us)
        else if loser p < target then
          binary ((lf,lp) : ls) (f,p) (uf,up) us
        else
          binary ls (lf,lp) (f,p) ((uf,up) : us)
      where
         f = (lf + uf) / 2.0
         p = entry f
         loser = case winnerFH of Player1 -> snd ; Player2 -> fst

    entry fuzz = (probFox,probHounds)
      where
        probFox = prob fuzz Player1 (getPositionFH (fst initialPositionsFH))
        probHounds = prob fuzz Player2 (getPositionFH (snd initialPositionsFH))

    prob fuzz spl (pl,p) =
        fst $ Strategy.probWinWith FH.game spl adv Map.empty pl p
      where
        adv = FH.strategy fuzz (Game.turn spl)

fuzzFH :: Prob
fuzzFH = fst fuzzTableFH

showFuzzTableFH :: (Prob,[(Prob,(Prob,Prob))]) -> String
showFuzzTableFH (fuzz,rows) =
    showFuzz fuzz ++ "\n" ++
    ppTable
      ([] :
       ["Fuzz\nfactor",
        "Fox wins\nfrom\n" ++ ifp ++ "\nposition",
        "Hounds\nwin\nfrom\n" ++ ihp ++ "\nposition"] :
       [] :
       map showRow rows ++
       [[]])
  where
    (ifp,ihp) = initialPositionsFH
    showRow (f,(pf,ph)) = [showFuzz f, showProb pf, showProb ph]
    showFuzz p = showFFloat (Just 6) p ""

maxIntCdfFH :: Integer
maxIntCdfFH = 100000

posEntryFH :: (Player,FH.Pos) -> RowFH
posEntryFH (pl,p) = (FH.posToIdx p, w, fb, fbm, moves mvs)
  where
    w = (FH.winningForFox pl p, FH.winDepth pl p)
    fb = Game.evalUnsafe FH.foxBox pl p
    fbm = Game.evalUnsafe FH.maxFoxBox pl p
    mvs = FH.moveDist fuzzFH pl p

    moves = fst . mapLR cdf 0.0 . map snd . List.sort . map posIdx
      where
        cdf s (q,pr) = let s' = s + pr in ((q, round (s' * m)), s')
        posIdx (q,pr) = (FH.coordToSquare (moved q), (FH.posToIdx q, pr))
        moved = Set.findMin . Set.difference (pieces p) . pieces
        pieces q = Set.insert (FH.fox q) (FH.hounds q)
        m = fromInteger maxIntCdfFH

posTableFH :: [RowFH]
posTableFH = map posEntryFH $ Map.keys FH.solution

showPosEntryFH :: RowFH -> String
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

writePosTableFH :: FilePath -> [RowFH] -> IO ()
writePosTableFH file entries = withFile file WriteMode $ \h ->
    mapM_ (hPutStrLn h . showPosEntryFH) entries

analyzeFH :: IO ()
analyzeFH = do
    putStrLn $ "Board size: " ++ dimFH
    putStrLn $ "Reachable positions: " ++ ppInteger reachableFH
    putStrLn $ "Reachable position index range: " ++ show reachableIdxFH
    putStrLn $ "Possible games: " ++ ppHugeInteger FH.gamesInitial
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
    putStrLn $ "Perfect game (" ++ FH.ppEval solutionFH ++ "):"
    putStr $ Game.ppPlay (FH.perfectPlay Player1 FH.initial)
    putStrLn ""
    putStr $ ppBestStudyFH Player1
    putStrLn ""
    putStr $ ppBestStudyFH Player2
    putStrLn ""
    putStrLn $ "Incorrect position evaluations: " ++ showIncorrectPositionEvaluationsFH incorrectPositionEvaluationsFH
    putStrLn $ "FoxBox strategy failure positions: " ++ showStrategyFailFH foxBoxStrategyFailFH
    putStrLn $ "Positions violating infinite maximum FoxBox iff winning for Fox: " ++ showInfiniteMaxFoxBoxFH infiniteMaxFoxBoxFH
    putStrLn ""
    if FH.boardSize > 8 then return () else do
        putStrLn $ "Win probabilities against strategies of different depths:"
        putStrLn $ showProbDepthFH probDepthFH
        putStr $ "Fairest fuzz factor: "
        putStrLn $ showFuzzTableFH fuzzTableFH
        putStr $ "Creating game database in " ++ dbFH ++ ":"
        writePosTableFH dbFH posTableFH
        putStrLn $ " " ++ show (length posTableFH) ++ " rows"

-------------------------------------------------------------------------------
-- Queen & Pawns
-------------------------------------------------------------------------------

dimQP :: String
dimQP = let n = show QP.boardSize in n ++ "x" ++ n

reachableQP :: Int
reachableQP = Game.reachable QP.solution

reachableIdxQP :: (QP.Idx,QP.Idx)
reachableIdxQP = (QP.unPos a, QP.unPos b)
  where
    ((_,a),_) = Map.findMin QP.solution
    ((_,b),_) = Map.findMax QP.solution

getPositionQP :: String -> (Player,QP.Pos)
getPositionQP "initial" = (Player1,QP.initial)
getPositionQP "opposite" = QP.opposite
getPositionQP _ = error "unknown position"

ppPositionQP :: String -> String
ppPositionQP s =
    sp ++ ":\n" ++
    Game.ppPlayerPosition pl p ++
    "Evaluation: " ++ QP.ppEval ev ++ "\n" ++
    "Index: " ++ show idx
  where
    sp = ucfirst s ++ " position"
    ev = Game.evalUnsafe QP.solution pl p
    idx = QP.unPos p
    (pl,p) = getPositionQP s

ppBestStudyQP :: Player -> String
ppBestStudyQP pl =
    case Game.bestStudies stdy of
      [] -> "No good " ++ spl ++ " studies"
      ((_,p) : _) -> spl ++ " to move and win:\n" ++
                     Game.ppPlay (Game.criticalPath QP.game stdy pl p)
  where
    stdy = QP.study pl
    spl = QP.ppPlayer pl

analyzeQP :: IO ()
analyzeQP = do
    putStrLn $ "Board size: " ++ dimQP
    putStrLn $ "Reachable positions: " ++ ppInteger reachableQP
    putStrLn $ "Reachable position index range: " ++ show reachableIdxQP
    putStrLn $ "Possible games: " ++ ppHugeInteger QP.gamesInitial
    putStrLn ""
    putStrLn $ ppPositionQP "initial"
    putStrLn ""
    putStrLn $ ppPositionQP "opposite"
    putStrLn ""
    putStrLn $ "Perfect game (" ++ QP.ppEval (QP.evalInitial QP.solution) ++ "):"
    putStr $ Game.ppPlay (QP.perfectPlay Player1 QP.initial)
    putStrLn ""
    putStr $ ppBestStudyQP Player1
    putStrLn ""
    putStr $ ppBestStudyQP Player2

-------------------------------------------------------------------------------
-- Top-level
-------------------------------------------------------------------------------

lineLength :: Int
lineLength = 75

games :: [(String, String, IO ())]
games = [("NC", "Noughts & Crosses", analyzeNC),
         ("FH", "Fox & Hounds", analyzeFH),
         ("QP", "Queen & Pawns", analyzeQP)]

usage :: String -> a
usage err =
    error $ err ++ "\n" ++ info
  where
    info =
      "Usage: solve GAME\n" ++
      "where GAME is one of the following:" ++ concatMap gameOpt games
    gameOpt (o,n,_) = "\n  " ++ o ++ " : " ++ n

main :: IO ()
main = do
    args <- Environment.getArgs
    let opt =  case args of
                    [] -> usage "no arguments"
                    [o] -> o
                    _ -> usage "too many arguments"
    let (name,analyze) = case filter (\(o,_,_) -> o == opt) games of
                           [] -> usage "unknown game"
                           [(_,n,a)] -> (n,a)
                           _ -> error "duplicate game"
    ___
    putStrLn name
    putStrLn ""
    analyze
    ___
    return ()
  where
    ___ = putStrLn $ replicate lineLength '_'
