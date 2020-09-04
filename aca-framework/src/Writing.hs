module Writing where

import AcaComputation
import CscTypes
import Statistics
import Control.Monad.State
import Data.List (intersperse)
import System.Directory (createDirectoryIfMissing)

display :: Csc -> AcaComputation ()
display csc = do
  saveIntermediateCsc
  let cscStr = showSmallCsc csc
  io $ putStrLn $ "Partial CSC:"
  if (null cscStr)
    then do io $ putStrLn "\n  (Empty CSC)"
    else do io $ putStrLn cscStr

printTrivialCsc :: AcaComputation ()
printTrivialCsc = do
  io $ putStrLn "\nTrivial CSC; exiting."

showSmallCsc :: Csc -> String
showSmallCsc csc =
  concat $ map (\p ->
                  "\n" ++
                  "  Partition:\n" ++ 
                  "    Upper Bound:\n"++(showUpperBound p) ++
                  "\n" ++ 
                  "    Lower Bound:\n"++(showLowerBound p) ++
                  "    Assumptions:\n"++(showAssumptions p)
               ) (disjointPartitions csc)

showInterval :: CscPartition -> String
showInterval p =
  "\n" ++
  "  Partition:\n" ++ 
  "    Upper Bound:\n"++(showUpperBound p) ++
  "\n" ++ 
  "    Lower Bound:\n"++(showLowerBound p) ++
  "    Assumptions:\n"++(showAssumptions p)

showAssumptions :: CscPartition -> String
showAssumptions p =
  let ls = assumptions p
      indent = "      "
  in concat $ map (\c -> indent ++ (showMinimalConjunction c) ++ "\n") ls
            
showUpperBound :: CscPartition -> String
showUpperBound p =
  let indent = "      "
      up = upperBound p
      upperString = indent ++ (showMinimalConjunction $ upper up)
      negations = upperNegations up
      negationsString = concat $ intersperse " OR " (map showMinimalConjunction negations)
  in
    if (null negations)
      then upperString
      else upperString ++ " MINUS: " ++ negationsString

showLowerBound :: CscPartition -> String
showLowerBound p =
  let ls = lowerBound p
      indent = "      "
  in concat $ map (\c -> indent ++ (showMinimalConjunction c) ++ "\n") ls

saveIntermediateCsc :: AcaComputation ()
saveIntermediateCsc = do
  csc <- getCsc
  iters <- getIterations
  let tag = (show iters) ++ ".csc"
  writeDataStructureToFile csc tag

displayIteration :: AcaComputation ()
displayIteration = do
  p <- getProgram
  io $ putStrLn $ "** Iteration "++(show (iteration p))++" ************\n"
  return ()

displayConditioningMessage :: String -> AcaComputation()
displayConditioningMessage prog = do
  io $ putStrLn $ "Instrumenting "++prog
  io $ putStrLn " to block space covered by partial CSC.\n"
  return ()
  
displayFinal :: Csc -> AcaComputation Artifacts
displayFinal csc = do

  writeDataStructureToFile csc "csc"

  updateTotalTime
  updateStatistics
  s <- getStatistics
  writeDataStructureToFile s "stats"

  io $ putStrLn  "Final CSC:"
  io $ putStrLn $ (showSmallCsc csc) ++ "\n"

  iters <- getIterations
  let statisticsString = getStatisticsString s csc iters
  artifacts <- compileArtifacts csc statisticsString (round $ totalTime s)
  showStatistics <- statisticsFlag
  if (showStatistics)
    then do
      io $ putStrLn statisticsString
      return artifacts
    else return artifacts

compileArtifacts :: Csc -> String -> Int -> AcaComputation Artifacts
compileArtifacts csc statisticsString time = do
  let status = getExitStatus csc
  let summary = ExitSummary status time
  let smallCsc = showSmallCsc csc
  let artifacts = Artifacts summary smallCsc statisticsString
  return artifacts

getExitStatus :: Csc -> String
getExitStatus csc =
  let partitionSize = length (disjointPartitions csc)
  in
    if (partitionSize == (equivalentPartitionSize csc))
      then "exact"
      else if (movedToTop csc)
        then "top"
        else "gap"

getStatisticsString :: Statistics -> Csc -> Int -> String
getStatisticsString s csc iters =
  " Total      time (s) : "++(show ((round :: (RealFrac a) => a -> Integer) $ totalTime s))++"\n\
  \ Possible   time (s) : "++(show ((round :: (RealFrac a) => a -> Integer) $ possibleTime s))++"\n\
  \ Definite   time (s) : "++(show ((round :: (RealFrac a) => a -> Integer) $ definiteTime s))++"\n\
  \ Generalize time (s) : "++(show ((round :: (RealFrac a) => a -> Integer) $ generalizeTime s))++"\n\
  \ Widening   time (s) : "++(show ((round :: (RealFrac a) => a -> Integer) $ wideningTime s))++"\n\
  \ Counting   time (s) : "++(show ((round :: (RealFrac a) => a -> Integer) $ countingTime s))++"\n\n\
  \ Iterations : "++(show (iters-1))++"\n\
  \ Generalizations : "++(show $ generalizeCount s)++"\n\
  \ Solver calls : "++(show $ solverCallCount s)++"\n\n\
  \ Total partitions : "++(show (length $ disjointPartitions csc))++"\n\
  \ Partitions without gap : "++(show (equivalentPartitionSize csc))++"\n\
  \ Conjuncts sliced away : "++(show $ conjunctsSlicedAway s)++"\n\n"

data ExitSummary = ExitSummary {
  exitStatus :: String,
  exitTime :: Int
  } deriving (Eq, Show)

data Artifacts = Artifacts {
  exitSumm :: ExitSummary,
  cscString :: String,
  statsString :: String
  } deriving (Eq, Show)

writeDataStructureToFile :: (Show a) => a -> String -> AcaComputation ()
writeDataStructureToFile ds tag = do
  st <- get
  let p = stateProgram st
  logPre <- getLogPrefix
  let path = logPre ++ "/logs_alpaca/" ++ (pName p)
  io $ createDirectoryIfMissing True path
  let filePath = path ++ "/" ++ (pName p) ++ "." ++ tag
  io $ writeFile filePath (show ds)

writeArtifacts :: Artifacts -> AcaComputation ()
writeArtifacts (Artifacts summary cscStr statisticsString) = do
  prefix <- getBaseLogPath
  writeSuccessExitSummary summary prefix
  writeHumanCsc cscStr prefix 
  writeHumanStats statisticsString prefix

writeSuccessExitSummary :: ExitSummary -> FilePath -> AcaComputation ()
writeSuccessExitSummary (ExitSummary status time) prefix = do
  let summaryString = status++"\n"++(show time)++"\n"
  let exitSummaryFile = prefix++"/exit.summary"
  io $ appendFile exitSummaryFile summaryString

writeHumanCsc :: String -> FilePath -> AcaComputation ()
writeHumanCsc cscStr prefix = do
  let humanCscFile = prefix++"/final.csc"
  io $ writeFile humanCscFile cscStr

writeHumanStats :: String -> FilePath -> AcaComputation ()
writeHumanStats statsStr prefix = do
  let humanStatsFile = prefix++"/statistics"
  io $ writeFile humanStatsFile statsStr

