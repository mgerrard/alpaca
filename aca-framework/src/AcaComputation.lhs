\begin{code}
module AcaComputation where

import Portfolio
import CscTypes
import Statistics
import Data.Time
import Data.List (sort, sortOn, intercalate, intersperse, isInfixOf, partition)
import Data.List.Utils hiding (merge)
import Language.C
import Numeric 
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map

type AcaComputation = StateT AcaState IO

data AcaState = AcaState {
  stateProgram   :: Program,
  stateCsc       :: Csc,
  statePortfolio :: Portfolio,
  stateDebug     :: DebugMode,
  stateStats     :: Statistics,
  printStats     :: Bool,
  stopEarly      :: Bool,
  blockValidPath :: Bool,
  exitStrategy   :: ExitStrategy,
  genExitStrat   :: ExitStrategy,
  runInSequence  :: Bool,
  runningLog     :: FilePath,
  logPrefix      :: String,
  exitSummary    :: FilePath,
  octagon        :: Bool,
  partitionBound :: Int,
  mergeLength    :: Int,
  genStrategy    :: GenStrategy,
  dseTool        :: DseTool,
  makeCud        :: Bool,
  chewCud        :: String,
  dockerPort     :: Bool,
  minusAca       :: Bool
  } deriving (Show)

data Program = Program {
  sourcePath  :: String,
  ast         :: CTranslUnit,
  pName       :: String,
  iteration   :: Int,
  iterLogPath :: String}
  deriving (Show, Eq)

data Log = Log {
  cscHistory :: [Csc]
  }

data Counter = Counter {
  cache :: Map.Map Conjunction CounterResult,
  port :: Int
  }

-- Helper methods

isNewEvidence :: Csc -> PieceOfEvidence -> Bool
isNewEvidence _ (LegitimateEvidence (AnalysisWitness _ _ _ True _ _ _) _ _) = True
isNewEvidence csc (SpuriousEvidence _ spuriousEvidence _) =
  let subspace = conjunction spuriousEvidence
  in (not $ null subspace) && (not $ subspace `elem` (spuriousSpace csc))
isNewEvidence csc e =
  let subspace = (conjunction . characterization) e
      lowerBounds = concat $ map lowerBound (disjointPartitions csc)
  in not $ subspace `elem` lowerBounds

isNonspuriousEvidence :: PieceOfEvidence -> Bool
isNonspuriousEvidence (SpuriousEvidence _ _ _) = False
isNonspuriousEvidence _ = True

isNewSpuriousEvidence :: Csc -> PieceOfEvidence -> Bool
isNewSpuriousEvidence _ (SpuriousEvidence _ _ _) = True
isNewSpuriousEvidence _ _ = False

existsSafetyProof :: EvidenceCollection -> Bool
existsSafetyProof ws = any isLegitimateSafetyWitness ws

isLegitimateSafetyWitness :: PieceOfEvidence -> Bool
isLegitimateSafetyWitness (LegitimateEvidence witness _ _) = isSafe witness
isLegitimateSafetyWitness _ = False

-- No response from directed CIVL is producing empty conjunction;
-- this is not trivially false.
triviallyFalse :: EvidenceCollection -> Bool
triviallyFalse ws = any isLegitimateNullCharacterization ws

isLegitimateNullCharacterization :: PieceOfEvidence -> Bool
isLegitimateNullCharacterization (SpuriousEvidence _ _ _) = False
isLegitimateNullCharacterization e = (null . conjunction . characterization) e

getBaseLogPath :: AcaComputation FilePath
getBaseLogPath = do
  st <- get
  let n = pName $ stateProgram st
  let p = logPrefix st
  return $ p++"/logs_alpaca/"++n

getDebugMode :: AcaComputation DebugMode
getDebugMode = do
  st <- get
  return $ stateDebug st

getExitStrategy :: AcaComputation ExitStrategy
getExitStrategy = do
  st <- get
  return $ exitStrategy st

getGenExitStrategy :: AcaComputation ExitStrategy
getGenExitStrategy = do
  st <- get
  return $ genExitStrat st

shouldBlockValid :: AcaComputation Bool
shouldBlockValid = do
  st <- get
  return $ blockValidPath st

getCsc :: AcaComputation Csc
getCsc = do
  st <- get
  return $ stateCsc st

updateCsc :: Csc -> AcaComputation ()
updateCsc csc = do
  st <- get
  put $ st { stateCsc = csc }

getProgram :: AcaComputation Program
getProgram = do
  st <- get
  return $ stateProgram st

getPortfolio :: AcaComputation Portfolio
getPortfolio = do
  st <- get
  return $ statePortfolio st

io :: IO a -> StateT AcaState IO a
io = liftIO

statisticsFlag :: AcaComputation Bool
statisticsFlag = do
  st <- get
  return $ printStats st

getIterations :: AcaComputation Int
getIterations = do
  st <- get
  -- increment by one because we zero index
  return $ succ $ iteration $ stateProgram st

getOctagonalize :: AcaComputation Bool
getOctagonalize = do
  st <- get
  return $ octagon st

-- Statistics helpers

startPossibleTime :: AcaComputation ()
startPossibleTime = do
  st <- get
  let s = stateStats st
  start <- io $ getCurrentTime
  let s' = s { possibleStart = start }
  put $ st { stateStats = s' }

endPossibleTime :: AcaComputation ()
endPossibleTime = do
  st <- get
  let s = stateStats st
  end <- io $ getCurrentTime
  let start              = possibleStart s
      additionalTime     = realToFrac $ diffUTCTime end start
      accumulatedTime    = possibleTime s
  let s' = s { possibleTime = (accumulatedTime + additionalTime) }
  put $ st { stateStats = s' }
  return ()

startDefiniteTime :: AcaComputation ()
startDefiniteTime = do
  st <- get
  let s = stateStats st
  start <- io $ getCurrentTime
  let s' = s { definiteStart = start }
  put $ st { stateStats = s' }

endDefiniteTime :: AcaComputation ()
endDefiniteTime = do
  st <- get
  let s = stateStats st
  end <- io $ getCurrentTime
  let start              = definiteStart s
      additionalTime     = realToFrac $ diffUTCTime end start
      accumulatedTime    = definiteTime s
  let s' = s { definiteTime = (accumulatedTime + additionalTime) }
  put $ st { stateStats = s' }
  return ()

startGeneralizeTime :: AcaComputation ()
startGeneralizeTime = do
  st <- get
  let s = stateStats st
  start <- io $ getCurrentTime
  let s' = s { generalizeStart = start }
  put $ st { stateStats = s' }

endGeneralizeTime :: AcaComputation ()
endGeneralizeTime = do
  st <- get
  let s = stateStats st
  end <- io $ getCurrentTime
  let start              = generalizeStart s
      additionalTime     = realToFrac $ diffUTCTime end start
      accumulatedTime    = generalizeTime s
  let s' = s { generalizeTime = (accumulatedTime + additionalTime) }
  put $ st { stateStats = s' }
  return ()

startWideningTime :: AcaComputation ()
startWideningTime = do
  st <- get
  let s = stateStats st
  start <- io $ getCurrentTime
  let s' = s { wideningStart = start }
  put $ st { stateStats = s' }

endWideningTime :: AcaComputation ()
endWideningTime = do
  st <- get
  let s = stateStats st
  end <- io $ getCurrentTime
  let start              = wideningStart s
      additionalTime     = realToFrac $ diffUTCTime end start
      accumulatedTime    = wideningTime s
  let s' = s { wideningTime = (accumulatedTime + additionalTime) }
  put $ st { stateStats = s' }
  return ()

startCountingTime :: AcaComputation ()
startCountingTime = do
  st <- get
  let s = stateStats st
  start <- io $ getCurrentTime
  let s' = s { countingStart = start }
  put $ st { stateStats = s' }

endCountingTime :: AcaComputation ()
endCountingTime = do
  st <- get
  let s = stateStats st
  end <- io $ getCurrentTime
  let start              = countingStart s
      additionalTime     = realToFrac $ diffUTCTime end start
      accumulatedTime    = countingTime s
  let s' = s { countingTime = (accumulatedTime + additionalTime) }
  put $ st { stateStats = s' }
  return ()

updateTotalTime :: AcaComputation ()
updateTotalTime = do
  s <- getStatistics
  let start = totalStart s
  end <- io $ getCurrentTime
  let time = realToFrac $ diffUTCTime end start
  let s' = s { totalTime = time }
  st <- get
  put $ st { stateStats = s' }

incrementGeneralizeCount :: AcaComputation ()
incrementGeneralizeCount = do
  st <- get
  let s = stateStats st
  let count = generalizeCount s
  let s' = s { generalizeCount = (count + 1) }
  put $ st { stateStats = s' }

increaseSolverCallCount :: Int -> AcaComputation ()
increaseSolverCallCount num = do
  st <- get
  let s = stateStats st
  let count = solverCallCount s
  let s' = s { solverCallCount = (count + num) }
  put $ st { stateStats = s' }

updateStatistics :: AcaComputation ()
updateStatistics = do
  st <- get; s <- getStatistics; 
  i <- getIterations; csc <- getCsc
  let totalSliced = sumSliceCounts csc
  let s' = s { iterationStat = i,
               partitionCount = length $ disjointPartitions csc,
               exactPartitionCount = equivalentPartitionSize csc,
               conjunctsSlicedAway = totalSliced}
  put $ st { stateStats = s' }

sumSliceCounts :: Csc -> Int
sumSliceCounts _ = 0
{-
TO REWORK

sumSliceCounts (Csc partitions _ _ _ _) =
  let lowers = concat $ map lowerBound partitions
      counts = map numberSlicedAway lowers
  in foldl (+) 0 counts
-}

getStatistics :: AcaComputation Statistics
getStatistics = do
  st <- get
  return $ stateStats st

getParallelism :: AcaComputation Parallelism
getParallelism = do
  st <- get
  if runInSequence st
    then return InSequence
    else return InParallel

logPortfolioRun :: EvidenceCollection -> AcaComputation ()
logPortfolioRun evidence = do
  iter <- getIterations
  let summary = portfolioRunSummary evidence iter
  base <- getBaseLogPath
  let portfolioLog = base ++ "/portfolio.log"
  io $ appendFile portfolioLog summary

runSummary :: EvidenceCollection -> AcaComputation String
runSummary evidence = do
  iter <- getIterations
  let summary = portfolioRunSummary evidence iter
  return summary

portfolioRunSummary :: EvidenceCollection -> Int -> String
portfolioRunSummary evidence _ =
  let toolSummaries = collectSummaries evidence 
      summary = intercalate "\n" toolSummaries
  in summary++"\n"

collectSummaries :: EvidenceCollection -> [String]
collectSummaries [] = ["No new reachability evidence found. Generalizing."]
collectSummaries e = sort $ map summarizePieceOfEvidence e

summarizePieceOfEvidence :: PieceOfEvidence -> String
summarizePieceOfEvidence (LegitimateEvidence (AnalysisWitness a _ _ True _ _ _) _ _) = "\n"++(show a)++" declares unreachability.\n\n_ Terminating ALPACA _________________"
summarizePieceOfEvidence (LegitimateEvidence (AnalysisWitness a _ _ _ _ _ _) _ _) = "\n"++(show a)++" found reachability evidence.\n(Adding reachability condition to CSC.)"
summarizePieceOfEvidence (SpuriousEvidence (AnalysisWitness a _ _ _ _ _ _) _ _) = (show a)++" reported spurious evidence."
summarizePieceOfEvidence (EmptyEvidence (AnalysisWitness a _ _ _ _ _ _)) = (show a)++"'s witness did not produce any evidence."
summarizePieceOfEvidence TrivialEvidence = "Trivial evidence reported (all inputs reach psi-state)"

setupRunConfiguration :: Maybe Int -> AcaComputation RunConfiguration
setupRunConfiguration tag = do
  debugMode <- getDebugMode; exitStrat <- getExitStrategy;
  bValid <- shouldBlockValid; parallelism <- getParallelism
  logPre <- getLogPrefix; exitF <- getExitSummaryFile; dTool <- getDseTool
  dockerFlag <- getDockerFlag
  return (RunConfiguration parallelism debugMode tag exitStrat bValid logPre exitF dTool dockerFlag)

getDockerFlag :: AcaComputation Bool
getDockerFlag = do
  st <- get
  return $ dockerPort st

getDseTool :: AcaComputation DseTool
getDseTool = do
  st <- get
  return $ dseTool st

getExitSummaryFile :: AcaComputation FilePath
getExitSummaryFile = do
  st <- get;
  return $ exitSummary st

getPartitionBound :: AcaComputation Int
getPartitionBound = do
  st <- get
  return $ partitionBound st
  
getMergeLength :: AcaComputation Int
getMergeLength = do
  st <- get
  return $ mergeLength st

setTerminationFlag :: AcaComputation ()
setTerminationFlag = do
  st <- get; csc <- getCsc
  let csc' = csc { searchFlag = Terminate }
  put $ st { stateCsc = csc' }

getLogPrefix :: AcaComputation String
getLogPrefix = do
  st <- get
  return (logPrefix st)

getGenStrategy :: AcaComputation GenStrategy
getGenStrategy = do
  st <- get
  return (genStrategy st)

writeExitSummary :: FilePath -> String -> IO ()
writeExitSummary exitFile message = do
  appendFile exitFile ("error\n"++message)

trueConjunct :: Conjunct
trueConjunct = Conjunct (CConst (CIntConst (cInteger 1) undefNode))

falseConjunct :: Conjunct
falseConjunct = Conjunct (CConst (CIntConst (cInteger 0) undefNode))

trueConjunction :: Conjunction
trueConjunction = [trueConjunct]

falseConjunction :: Conjunction
falseConjunction = [falseConjunct]

emptyConjunction :: Conjunction
emptyConjunction = []

makeTrueUpper :: UpperBound
makeTrueUpper = UpperBound trueConjunction [emptyConjunction]

makeFalseUpper :: UpperBound
makeFalseUpper = UpperBound falseConjunction [emptyConjunction]

makeTrueLower :: LowerBound
makeTrueLower = [trueConjunction]

makeFalseLower :: LowerBound
makeFalseLower = [falseConjunction]

upperBounds :: Csc -> [Conjunction]
upperBounds csc = map (upper . upperBound) $ disjointPartitions csc

lastAddedConjunction :: Csc -> Conjunction
lastAddedConjunction csc = upper $ upperBound $ last $ disjointPartitions csc

shortestConjunction :: Csc -> Conjunction
shortestConjunction csc =
  let uppers = upperBounds csc
  in (head . sortOn length) uppers

shortestConjWithEq :: Csc -> Conjunction
shortestConjWithEq csc =
  let uppers = upperBounds csc
      (eqUppers, diseqUppers) = partition hasEquality uppers
      eqUppers' = sortOn length eqUppers
      diseqUppers' = sortOn length diseqUppers
      uppers' = (eqUppers'++diseqUppers')
      -- don't want to return a singleton, because this will
      -- immediately get generalized to Top
      (singles, nonsingles) = partition isSingleton uppers'
      uppers'' = nonsingles++singles
  in head uppers''

isSingleton :: Conjunction -> Bool
isSingleton c = (length c) == 1

hasEquality :: Conjunction -> Bool
hasEquality c =
  let cj = map show c
      cj' = intercalate " && " cj
  in " == " `isInfixOf` cj'

movedToTop :: Csc -> Bool
movedToTop csc = 
  let dps = disjointPartitions csc
      up = upper $ upperBound (head dps)
      firstConjunct = head up
  in
    if firstConjunct == trueConjunct
      then True
      else False

cMinInt :: Int
cMinInt = (-2147483648)

cMaxInt :: Int
cMaxInt = 2147483647

moveToTop :: AcaComputation ()
moveToTop = do
  csc <- getCsc
  st <- get
  let lowerBounds = collectLowerBounds csc
      assumes = collectAssumptions csc
      trueUpper = makeTrueUpper
      topMinusLowerBounds = CscPartition trueUpper lowerBounds assumes
      csc' = csc { disjointPartitions = [topMinusLowerBounds] }
  put $ st { stateCsc = csc' }
  {- update log -}
  return ()

collectLowerBounds :: Csc -> [Conjunction]
collectLowerBounds csc = concat $ map lowerBound $ disjointPartitions csc

collectAssumptions :: Csc -> [Conjunction]
collectAssumptions csc = concat $ map assumptions $ disjointPartitions csc

getRunningLog :: AcaComputation FilePath
getRunningLog = do
  st <- get
  return (runningLog st)

updateLog :: String -> AcaComputation ()
updateLog s = do
  f <- getRunningLog
  updateLogWorker f s

updateLogWorker :: FilePath -> String -> AcaComputation ()
updateLogWorker f s = io $ appendFile f s

formatFloatN :: RealFloat a => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

makeResultLog :: [PieceOfEvidence] -> String
makeResultLog ws = concat $ map showEvidenceSummary ws

makeGenResultLog :: [(PieceOfEvidence, Conjunction, Float)] -> String
makeGenResultLog es = concat $ map (\(e,c,t)->(showConj c)++(reportTime t)++" "++(showEvidenceSummary e)) es

reportTime :: Float -> String
reportTime f = " (tentative CSC widening took "++(showFl f)++"s)\n"

showConj :: Conjunction -> String
showConj c = "on conjunction:\n "++(showMinimalConjunction c)++"\n"

showFl :: Float -> String
showFl f = formatFloatN f 4
  
makeSubspaceMsg :: Subspace -> Float -> Float -> String
makeSubspaceMsg space t pt = " "++(showMinimalConjunction $ conjunction space)++"\n (parsing witness took "++(showFl pt)++"s)\n (directed symex took "++(showFl t)++"s)\n"

makeSubspaceMsg' :: Subspace -> Float -> Float -> String
makeSubspaceMsg' space t pt = " "++(showMinimalConjunction $ conjunction space)++"\n (concrete witness -> CPA -> parsing witness took "++(showFl pt)++"s)\n (directed symex took "++(showFl t)++"s)\n"

witnessIsConcrete :: Analyzer -> Bool
witnessIsConcrete (Analyzer _ _ _ _ _ _ ConcreteInputs _ _) = True
witnessIsConcrete _ = False

showEvidenceSummary :: PieceOfEvidence -> String
showEvidenceSummary (LegitimateEvidence (AnalysisWitness a _ _ True _ t _) _ _) = (show a)++" proved unreachability after "++(showFl t)++"s\n\n"
showEvidenceSummary (LegitimateEvidence (AnalysisWitness a _ _ False _ t pt) ss ct) =
  if (witnessIsConcrete a)
    then (show a)++" found legitimate reachability after "++(showFl t)++"s:\n"++(makeSubspaceMsg' ss ct pt)++"\n"
    else (show a)++" found legitimate reachability after "++(showFl t)++"s:\n"++(makeSubspaceMsg ss ct pt)++"\n"
showEvidenceSummary (SpuriousEvidence (AnalysisWitness a _ _ _ _ t pt) ss ct) =
  if (witnessIsConcrete a)
    then (show a)++" found spurious reachability evidence after "++(showFl t)++"s\n"++(makeSubspaceMsg' ss ct pt)++"\n"
    else (show a)++" found spurious reachability evidence after "++(showFl t)++"s\n"++(makeSubspaceMsg ss ct pt)++"\n"
showEvidenceSummary TrivialEvidence = "CIVL reasoned that all paths lead to the psi-state (trivial reachability)\n"
showEvidenceSummary (EmptyEvidence (AnalysisWitness a _ _ _ _ t _)) = (show a)++" returned evidence without directives after "++(showFl t)++"s\n"

showMinimalConjunction :: Conjunction -> String
showMinimalConjunction cs = concat $ intersperse " && " (map (\(Conjunct c) -> "("++(simplifyCee $ show $ pretty c)++")")  cs)

showConjunction :: Conjunction -> String
showConjunction cs = concat $ intersperse " && " (map (\(Conjunct c) -> "("++(show $ pretty c)++")")  cs)

simplifyCee :: String -> String
simplifyCee = replace "aca_input_arr" "X"

checkForSafety :: Csc -> Csc
checkForSafety csc@(Csc dps a b c d) =
  if null dps
    then
      let safePartition = CscPartition makeFalseUpper makeFalseLower []
      in Csc [safePartition] a b c d
    else csc
\end{code}