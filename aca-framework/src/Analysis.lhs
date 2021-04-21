\begin{code}
module Analysis where

import Control.Exception
import Control.Monad.State
import Data.List.Split
import qualified Data.Map.Strict as Map
import Language.C.Syntax
import Language.C
import System.FilePath.Posix
import System.Posix.User
import System.Directory
import System.Process
import System.Environment (getEnv, setEnv)
import System.Exit
import System.Posix.Process
import Data.String.Utils (strip)
import Transformer
import CscTypes
import Solver
import Portfolio
import Parsing
import Writing
import Statistics
import AcaComputation
import Configuration
import RunPortfolio
import Data.Time
import Characterize
import LocalPaths

aca :: Program -> Csc -> AcaComputation Csc
aca program csc = do
  result <- exploreSubspace program csc
  case result of
    UnreachableEvidence -> do
      csc' <- enforceDisjointness csc
      lastWrites csc'
      return csc'
    (ReachableEvidence ev) -> do
      csc' <- characterize ev
      program' <- condition program csc'
      aca program' csc'
    NoEvidence -> do
      csc' <- generalize csc
      program' <- condition program csc'
      aca program' csc'

lastWrites :: Csc -> AcaComputation ()
lastWrites csc = do
  let csc' = checkForSafety csc
  artifacts <- displayFinal csc'
  writeArtifacts artifacts
  s <- getStatistics
  let t = ((realToFrac $ totalTime s)::Float)
  updateLog $ "\n***********************************************************\n"
  updateLog $ "terminating ALPACA after "++(showFl t)++"s\n"
  updateLog $ "***********************************************************\n\n"
  updateLog $ "Comprehensive State Characterization:\n"
  updateLog (showSmallCsc csc')
  covers <- coversDomain csc'
  if covers
    then do
      io $ putStrLn "! The upper bounds cover the entire domain.\n"
      updateLog "\n*** upper bounds cover the entire domain ***\n"
    else do
      updateLog "\n(upper bounds do NOT cover the entire domain)\n"
  cqaTransformations csc'
  finalTransformations csc'

finalTransformations :: Csc -> AcaComputation ()                
finalTransformations csc = do
  if (noGapsInPartitions csc)
    then do
      return ()
    else do
      let (Csc partitions _ _ _ _) = csc
      let gaps = (filter gapBetweenBounds) partitions
      let gapTuples = zip gaps [1..]
      mapM_ instrumentGapProgram gapTuples
      return ()

cqaTransformations :: Csc -> AcaComputation ()                
cqaTransformations (Csc partitions _ _ _ _) = do
  logPath <- getBaseLogPath
  let cqaPath = logPath++"/cqa"
  io $ createDirectoryIfMissing True cqaPath
  mapM_ (createIntervalArtifact cqaPath) (zip [1..] partitions)
  return ()

type Interval = CscPartition

createIntervalArtifact :: FilePath -> (Int, Interval) -> AcaComputation ()
createIntervalArtifact dir (k,interval) = do
  let intervalDir = dir++"/"++(show k)
  io $ createDirectoryIfMissing True intervalDir
  
  if gapBetweenBounds interval
    then do
      let intervalFile = intervalDir++"/noncoinciding_interval"
      io $ writeFile intervalFile (showInterval interval)
      writeIntervalProgram intervalDir (interval,k)
      return ()
    else do
      let intervalFile = intervalDir++"/coinciding_interval"
      io $ writeFile intervalFile (showInterval interval)
      return ()

noGapsInPartitions :: Csc -> Bool
noGapsInPartitions (Csc partitions _ _ _ _) =
  let gaps = (filter gapBetweenBounds) partitions
  in null gaps

provedUnreachability :: GeneralizeResult -> Bool
provedUnreachability (SafetySpace _) = True
provedUnreachability Top = True
provedUnreachability _ = False

exploreSubspace :: Program -> Csc -> AcaComputation ExplorationResult
exploreSubspace _ (Csc _ _ _ _ Terminate) = do
  displayIteration
  {- update log -}
  return UnreachableEvidence
exploreSubspace program csc = do
  displayIteration
  io $ putStrLn "Launching tool portfolio."
  p <- getProgram
  updateLog "\n---------------------------------------------------\n"
  updateLog $ "iteration "++(show (iteration p))++"\n"
  updateLog "---------------------------------------------------\n\n"
  thePortfolio <- getPortfolio
  runConfig <- setupRunConfiguration Nothing -- no generalize tag
  exitFile <- getExitSummaryFile
  startPossibleTime
  start <- io $ getCurrentTime
  results <- io $ (runPortfolio program thePortfolio csc runConfig)
                  `onException`
                  (writeExitSummary exitFile "Exploring subspace")
  endPossibleTime
  end <- io $ getCurrentTime

  {- Stop if just dumping Vicuna data -}
  st <- get
  if (minusAca st)
    then io $ exitImmediately ExitSuccess
    else return ()

  let searchTime = formatFloatN ((realToFrac $ diffUTCTime end start)::Float) 4
  {- update log given the results -}
  let r = makeResultLog results
  updateLog r
  
  logPortfolioRun results
  summary <- runSummary results
  io $ putStrLn summary
  
  let newResults = filter (isNewEvidence csc) results
  let shouldStop = stopEarly st

  if shouldStop
    then do
      io $ classifyEarlyResults newResults
      assert False (return NoEvidence)
    else if null newResults
           then do
             updateLog $ "no new reachability evidence found after "++searchTime++"s:\n -> generalizing\n\n"
             return NoEvidence
           else return $ classifyResults newResults

classifyResults :: EvidenceCollection -> ExplorationResult
classifyResults ev = do
  if (existsSafetyProof ev)
    then UnreachableEvidence
    else ReachableEvidence ev

safetyDeconstructor :: PieceOfEvidence -> String
safetyDeconstructor (LegitimateEvidence (AnalysisWitness a _ _ _ _ _ _) _ _) = "  "++(show (analysisTool a))
safetyDeconstructor _ = assert False ""

classifyEarlyResults :: EvidenceCollection -> IO ()
classifyEarlyResults [] = putStrLn "Early stopping: Found no results"
classifyEarlyResults ev = 
  if (existsSafetyProof ev)
    then putStrLn "\nEarly stopping: Some verifier found UnreachableEvidence"
    else if (triviallyFalse ev)
      then putStrLn "\nEarly stopping: CIVL found TrivialEvidence (all paths lead to failure)."
      else putStrLn $ "\nEarly stopping: SUCCESS: some tool found evidence! \n\n"++(show ev)

condition :: Program -> Csc -> AcaComputation Program
condition p@(Program _ _ n iter _) csc = do
  display csc
  logPre <- getLogPrefix
  condStart <- io $ getCurrentTime
  let ast' = cscAstTransform p csc False
      progStr = show $ pretty ast'
      iter' = iter + 1
      iterTag = "iter." ++ (show iter')
      logPath = logPre ++ "/logs_alpaca/" ++ n ++ "/" ++ iterTag ++ "/"
      filePath = logPath ++ n ++ "." ++ iterTag ++ ".c"
  io $ createDirectoryIfMissing True logPath
  io $ writeFile filePath progStr
  displayConditioningMessage (n++".c")
  let program' = (Program filePath ast' n iter' logPath)
  st <- get
  put $ st { stateProgram = program' }
  condEnd <- io $ getCurrentTime
  let condTime = formatFloatN ((realToFrac $ diffUTCTime condEnd condStart)::Float) 4
  {- update log -}
  updateLog $ "instrumentation took "++condTime++"s\n"
  return program'

initialInstrumentation :: Program -> Csc -> FilePath -> Bool -> IO ()
initialInstrumentation p@(Program _ _ n iter _) csc logPre minAca = do
  let ast' = cscAstTransform p csc minAca
      progStr = show $ pretty ast'
      iterTag = "iter." ++ (show iter)
      logPath = logPre ++ "/logs_alpaca/" ++ n ++ "/" ++ iterTag ++ "/"
      filePath = logPath ++ n ++ "." ++ iterTag ++ ".c"
  createDirectoryIfMissing True logPath
  writeFile filePath progStr
  return ()

instrumentGapProgram :: (CscPartition, Int) -> AcaComputation ()
instrumentGapProgram (part, partitionTag) = do
  p@(Program _ _ n _ _) <- getProgram
  csc <- getCsc
  logPre <- getLogPrefix
  let ast' = gapAstTransform p csc part
      progStr = show $ pretty ast'
      gapTag = "gap_instr"
      logPath = logPre ++ "/logs_alpaca/" ++ n ++ "/" ++ gapTag ++ "/"
      filePath = logPath ++ n ++ ".partitionWithGap." ++ (show partitionTag) ++ ".c"
  io $ createDirectoryIfMissing True logPath
  io $ writeFile filePath progStr
  return ()

writeIntervalProgram :: FilePath -> (CscPartition, Int) -> AcaComputation ()
writeIntervalProgram pre (interval, intervalTag) = do
  p@(Program _ _ n _ _) <- getProgram
  csc <- getCsc
  let ast' = gapAstTransform p csc interval
      progStr = show $ pretty ast'
      filePath = pre++"/"++n++".interval."++(show intervalTag)++".c"
  io $ writeFile filePath progStr
  return ()

cscAstTransform :: Program -> Csc -> Bool -> CTranslUnit
cscAstTransform (Program _ a _ _ _) csc False = updateTransform a csc
cscAstTransform (Program _ a _ _ _) csc True = a

gapAstTransform :: Program -> Csc -> CscPartition -> CTranslUnit
gapAstTransform (Program _ a _ _ _) csc p = restrictToGapTransform a csc p

searchMovedToTop :: GeneralizeResult -> Bool
searchMovedToTop Top = True
searchMovedToTop _ = False

initialCsc :: FilePath -> FilePath -> IO Csc
initialCsc _ "" = return $ Csc { disjointPartitions=[], inputCountMap=(Map.empty), inputTypeMap=(Map.empty), spuriousSpace=[], searchFlag=Search }
initialCsc _ cudFile = do
  cudStr <- readFile cudFile
  let cudLines = lines cudStr
      iCount = cudLines !! 1
      iType = cudLines !! 2
      iCountMap = (read iCount) :: CountMap
      iTypeMap = (read iType) :: TypeMap
  return $ Csc { disjointPartitions=[], inputCountMap=iCountMap, inputTypeMap=iTypeMap, spuriousSpace=[], searchFlag=Search }

writePathToSummaryFile :: FilePath -> FilePath -> String -> IO FilePath
writePathToSummaryFile f logPath target = do
  f' <- makeAbsolute f
  let summaryFile = logPath ++ "/exit.summary"
  if (target == "__VERIFIER_error")
    then writeFile summaryFile (f'++"\n")
    else writeFile summaryFile (f'++" (reachability of "++target++")\n")
  return summaryFile

writeConfigurationFile :: Configuration -> FilePath -> IO ()
writeConfigurationFile c prefix = do
  let configFile = prefix ++ "/config"
  writeFile configFile (show c)

checkFileExists :: FilePath -> IO ()
checkFileExists f = do
  fileExists <- doesFileExist f
  if (not fileExists)
    then error $ "oops, i can't find '"++f++"'"
    else return ()

checkDockerPermissions :: Bool -> IO ()
checkDockerPermissions False = return ()
checkDockerPermissions True = do
  uId <- getRealUserID
  if uId /= 0
    then error "user must be root to run docker"
    else return ()

deriveProperty :: String -> Property
deriveProperty "reachSafety" = ReachSafety
deriveProperty "memSafety" = MemSafety
deriveProperty "overflow" = OverflowSafety
deriveProperty p = error $ "sorry, I don't know the property: "++(show p)

runAca :: Configuration -> IO Csc
runAca c@(Configuration program d timeout selection gTimeout bValid ex gex logPre targetFunc partBound merLen genStrat cppFlags iTimeout exclusion dseT mkCud chCud dockerFlag minusAcaFlag prp) = do
  checkDockerPermissions dockerFlag
  checkFileExists program
  let prop = deriveProperty prp
  setLibraryEnvironmentVariable
  now <- getCurrentTime
  initProgram <- initialProgram program "" "main" logPre targetFunc cppFlags (dseChoice dseT) chCud minusAcaFlag
  end1 <- getCurrentTime

  let astReadTime = formatFloatN ((realToFrac $ diffUTCTime end1 now)::Float) 4
  {- update log -}
  initCsc <- initialCsc "" chCud
  startInstrumentation <- getCurrentTime
  initialInstrumentation initProgram initCsc logPre minusAcaFlag
  endInstrumentation <- getCurrentTime
  
  let prefix = logFilePrefix initProgram logPre
  let instrTime = formatFloatN ((realToFrac $ diffUTCTime endInstrumentation startInstrumentation)::Float) 4
  let logHandle = prefix++"/alpaca.log"
  appendFile logHandle $ "***********************************************************\n"  
  appendFile logHandle $ "launching ALPACA on "++(program)++"\n"
  appendFile logHandle $ "***********************************************************\n\n"  
  appendFile logHandle $ "constructing the AST took "++astReadTime++"s\n"
  appendFile logHandle $ "initial instrumentation took "++instrTime++"s\n"

  exitFile <- writePathToSummaryFile program prefix targetFunc
  writeConfigurationFile c prefix

  initPortfolio <- portfolio selection prop exclusion timeout gTimeout iTimeout
  let initStats = Statistics 0 now 0 now 0 now 0 now 0 now 0 now 0 0 0 0 0 0
  putStrLn $ "* Running ALPACA on "++program++"\n"

  evalStateT (aca initProgram initCsc) $
    AcaState
    { stateProgram   = initProgram
    , stateCsc       = initCsc
    , statePortfolio = initPortfolio
    , stateDebug     = debugMode d
    , stateStats     = initStats
    , printStats     = False
    , stopEarly      = False
    , blockValidPath = bValid
    , exitStrategy   = exitMode ex
    , genExitStrat   = exitMode gex
    , runInSequence  = False
    , runningLog     = logHandle
    , logPrefix      = logPre
    , exitSummary    = exitFile
    , octagon        = False
    , partitionBound = partBound
    , mergeLength    = merLen
    , genStrategy    = genMode genStrat
    , dseTool        = dseChoice dseT
    , makeCud        = mkCud
    , chewCud        = chCud
    , dockerPort     = dockerFlag
    , minusAca       = minusAcaFlag
    , stateProperty  = prop
    }

dseChoice :: String -> DseTool
dseChoice "cpa" = CpaSymExec
dseChoice "civl" = CivlSymExec
dseChoice s = error $ "sorry, i do not recognize the --dse option '"++s++"'. choose from (cpa | civl)."

setLibraryEnvironmentVariable :: IO ()
setLibraryEnvironmentVariable = do
  homeDir <- getEnv "HOME"
  let acaConfig = homeDir ++ "/.aca.config"
  configFileExists <- doesFileExist acaConfig
  if configFileExists
    then do
      path <- readFile acaConfig
      let path' = strip path
      setEnv "ACA_LIB" path'
      return ()
    else do
      putStrLn $ "Could not find "++acaConfig++". Aborting"
      putStrLn $ "If running in --docker mode, preserve HOME by running:"
      putStrLn $ "  sudo --preserve-env=HOME alpaca foo.c"
      assert False (return ())

debugMode :: String -> DebugMode
debugMode "quiet"      = Quiet
debugMode "full"       = Full
debugMode "slice"      = Slice
debugMode "analyzers"  = Analyzers
debugMode "direct"     = Direct
debugMode "generalize" = Generalize
debugMode _ = assert False Quiet

exitMode :: String -> ExitStrategy
exitMode "eager"   = Eager
exitMode "patient" = Patient
exitMode _ = assert False Eager

genMode :: String -> GenStrategy
genMode "pessimisticEq" = PessimisticEq
genMode "pessimisticDisEq" = PessimisticDisEq
genMode "optimisticEq" = OptimisticEq
genMode "optimisticDisEq" = OptimisticDisEq
genMode s = error $ "I don't know the generalization mode '"++s++"'. Please choose from 'pessimistic' or 'optimistic'."

clearExistingLogs :: String -> String -> FilePath -> IO ()
clearExistingLogs progName "" logPre = do
  let logs = logPre ++ "/logs_alpaca/" ++ progName
  logsExist <- doesDirectoryExist logs
  if logsExist
    then do removeDirectoryRecursive logs
    else do return ()
clearExistingLogs _ _ _ = return ()

removeStubbedLibraries :: [String] -> FilePath -> IO FilePath
removeStubbedLibraries libs p = do
  let libExprs = (concat $ map (\l->"/"++l++"/d;") libs)
  (_,out,_) <- readProcessWithExitCode "/bin/sed" [libExprs, p] ""
  let p' = p++".tmp"
  writeFile p' out
  return p'

runPreprocessor :: FilePath -> String -> IO FilePath
runPreprocessor p flags = do
  let libs = ["assert.h"]
  p' <- removeStubbedLibraries libs p
  let p'' = p ++ ".cpp.c"
  if null flags
    then do
      _ <- readProcessWithExitCode "cpp" [p', p''] ""
      return p''
    else do
      let flags' = splitOn " " flags
      _ <- readProcessWithExitCode "cpp" (flags'++[p', p'']) ""
      return p''

bareProgramName :: FilePath -> String -> String
bareProgramName p "__VERIFIER_error" = last $ splitOn "/" $ dropExtension p
bareProgramName p funcName =
  let baseName = last $ splitOn "/" $ dropExtension p
  in baseName++"_reach_"++funcName


wrapCudInMaybe :: String -> IO (Maybe String)
wrapCudInMaybe "" = return Nothing
wrapCudInMaybe cudFile = do
  cudStr <- readFile cudFile
  let cudLines = lines cudStr
      assump = cudLines !! 0
  return $ Just assump

initialProgram :: FilePath -> FilePath -> String -> FilePath -> String -> String -> DseTool -> String -> Bool -> IO Program
initialProgram p initCsc "main" logPre targetFunc cppFlags dTool cud minAca = do
  let iter = 1
  let n = bareProgramName p targetFunc
  let iterTag = "iter.1"
  let logPath = logPre ++ "/logs_alpaca/" ++ n ++ "/" ++ iterTag ++ "/"
  let filePath = logPath ++ n ++ "." ++ iterTag ++ ".c"
  clearExistingLogs n initCsc logPre
  p' <- runPreprocessor p cppFlags
  pAst <- getAst p'
  removeFile p'
  mCud <- wrapCudInMaybe cud
  let pAst' = twoPassTransform pAst targetFunc dTool mCud
  let prog = Program {
        sourcePath=filePath
      , ast=(if minAca then pAst else pAst')
      , pName=n
      , iteration=iter
      , iterLogPath=logPath -- set during call to instrument
      }
  return prog
{- Construct modular ACA program -}
initialProgram p initCsc funcName logPre targetFunc cppFlags dTool cud minAca = do
  let iter = 1
  let n = last $ splitOn "/" $ dropExtension p
  let name' = funcName ++ "_aca_" ++ n;
  let iterTag = "iter.1"
  let logPath = logPre ++ "/logs_alpaca/" ++ name' ++ "/" ++ iterTag ++ "/"
  let filePath = logPath ++ name' ++ "." ++ iterTag ++ ".c"
  clearExistingLogs name' initCsc logPre
  p' <- runPreprocessor p cppFlags
  pAst <- getAst p'
  pAst' <- initialTransform pAst funcName 
  let newHandle = name' ++ ".c"
  writeFile newHandle (show $ pretty pAst')
  pAst'' <- getAst newHandle
  mCud <- wrapCudInMaybe cud
  let pAst''' = twoPassTransform pAst'' targetFunc dTool mCud
  let prog = Program {
        sourcePath=filePath
      , ast=pAst'''
      , pName=name'
      , iteration=iter
      , iterLogPath=logPath -- set during call to instrument
      }
  return prog
\end{code}