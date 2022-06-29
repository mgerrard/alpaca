\begin{code}
{-# LANGUAGE PackageImports #-}
module RunPortfolio where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.List.Split
import Data.List.Utils hiding (merge)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import System.FilePath.Posix
import "Glob" System.FilePath.Glob
import System.FilePath.GlobPattern
import System.FilePath.Find
import System.Directory
import System.Process
import System.Exit
import System.Posix.Process
import Data.Maybe
import Data.List
import Data.Time
import CscTypes
import Portfolio
import Branch
import CivlParsing
import CpaParsing
import LocalPaths
import AcaComputation
import LaunchTool
import Reading

type Thread = Async (Maybe AnalysisWitness)

runPortfolio :: Program -> Portfolio -> Csc -> RunConfiguration -> IO EvidenceCollection
runPortfolio prog pfolio csc (RunConfiguration InParallel d tag exitStrat bValid logPre exitFile dTool prp isMinAca hasReach) = do
  {- update log : track total analysis runtime -}
  threads <- launchPortfolio prog d tag logPre exitFile dTool prp isMinAca hasReach pfolio 
  results <- pollUntilDone threads pfolio dTool exitStrat csc d bValid exitFile
  mapM_ uninterruptibleCancel threads
  return results
runPortfolio prog pfolio csc (RunConfiguration InSequence d tag _ bValid logPre exitFile dTool prp isMinAca hasReach) = do
  maybeWitnesses <- mapM (runAnalyzer prog d tag logPre exitFile dTool prp isMinAca hasReach) pfolio
  let witnesses = catMaybes maybeWitnesses
  maybeEvidence <- mapM (verifyWitness csc dTool d bValid exitFile) witnesses
  let results = catMaybes maybeEvidence
  return results

verifyWitness :: Csc -> DseTool -> DebugMode -> Bool -> FilePath -> AnalysisWitness -> IO (Maybe PieceOfEvidence)
verifyWitness csc dTool debug bValid exitFile witness = do
  if isUnreachableResult witness
    then return (Just (extractUnreachableResult witness))
    else do
      newlyClassified <- checkAnalysisWitness dTool debug bValid exitFile witness
      let newEvidence = isNewEvidence csc newlyClassified
      if newEvidence
        then return $ (Just newlyClassified)
        else return $ Nothing
        
launchPortfolio :: Program -> DebugMode -> Maybe Int -> FilePath -> FilePath -> DseTool -> Property -> Bool -> Bool -> Portfolio -> IO [Thread]
launchPortfolio prog d tag logPre ef dTool prp isMinAca hasReach pfolio = mapM (async . (runAnalyzer prog d tag logPre ef dTool prp isMinAca hasReach)) pfolio

pollUntilDone :: [Thread] -> Portfolio -> DseTool -> ExitStrategy -> Csc -> DebugMode -> Bool -> FilePath -> IO EvidenceCollection
pollUntilDone threads _ dTool strategy csc debug bValid exitFile = do
  let finished = []
  evidence <- pollLoop threads finished dTool strategy csc debug bValid exitFile
  return evidence

pollLoop :: [Thread] -> EvidenceCollection -> DseTool -> ExitStrategy -> Csc -> DebugMode -> Bool -> FilePath -> IO EvidenceCollection
pollLoop [] finished _ _ _ _ _ _ = return finished
pollLoop runningThreads finished dTool exitStrat csc debug bValid ef = do
  maybeResults <- mapM poll runningThreads
  if any isUnreachableThreadResult maybeResults
    then do
      let result = extractUnreachableThreadResult maybeResults
      return [result]
    else if any isJust maybeResults
      then do
        let (running', stopped) = partitionRunningAndFinished runningThreads maybeResults
        newlyClassified <- (checkResults stopped dTool debug bValid ef)
--        let finished' = finished ++ newlyClassified
        let newEvidence = filter (isNewEvidence csc) newlyClassified
        let finished' = finished ++ newEvidence
        if ((exitStrat == Eager) && (any isNonspuriousEvidence newEvidence))
          then return finished' 
          else pollLoop running' finished' dTool exitStrat csc debug bValid ef
      else do
        threadDelay 100000 -- poll every 0.1 seconds
        pollLoop runningThreads finished dTool exitStrat csc debug bValid ef

partitionRunningAndFinished :: [Thread] -> [ThreadResult] -> ([Thread], [AnalysisWitness])
partitionRunningAndFinished ts rs = 
  let tuples = zip rs ts
      (running, stopped) = partition (\y -> isNothing (fst y)) tuples
      running' = map snd running
      stopped' = map (extractAnalysisResult . fst) stopped
      stopped'' = catMaybes stopped'
  in (running', stopped'')

type ThreadResult = Maybe (Either SomeException (Maybe AnalysisWitness))

extractAnalysisResult :: ThreadResult -> Maybe AnalysisWitness
extractAnalysisResult (Just (Right result)) = result
extractAnalysisResult (Just (Left e)) = throw e
extractAnalysisResult _ = Nothing

isUnreachableThreadResult :: ThreadResult -> Bool
isUnreachableThreadResult (Just (Right (Just (AnalysisWitness _ _ _ True _ _ _)))) = True
isUnreachableThreadResult _ = False

isUnreachableResult :: AnalysisWitness -> Bool
isUnreachableResult (AnalysisWitness _ _ _ True _ _ _) = True
isUnreachableResult _ = False

extractUnreachableResult :: AnalysisWitness -> PieceOfEvidence
extractUnreachableResult unreachWitness = LegitimateEvidence unreachWitness emptySubspace 0

extractUnreachableThreadResult :: [ThreadResult] -> PieceOfEvidence
extractUnreachableThreadResult rs =
  let (Just wrappedResult) = Data.List.find isUnreachableThreadResult rs
      (Just (Right (Just unreachableWitness))) = wrappedResult
      unreachEvidence = LegitimateEvidence unreachableWitness emptySubspace 0
  in unreachEvidence

updatePortfolio :: Portfolio -> [AnalysisWitness] -> Portfolio
updatePortfolio p witnesses =
  let pWithWitness = map analyzer witnesses
      pWithoutWitness = p \\ pWithWitness
  in pWithWitness ++ pWithoutWitness

launchSeahorn :: FilePath -> Analyzer -> Int -> FilePath -> Maybe Int -> FilePath -> Bool -> IO String
launchSeahorn cFile a t oDir _ logPre debug = do
  currDir <- getCurrentDirectory
  let pre = absolutePrefix logPre currDir
  let outDir = absoluteOutDir pre oDir
  let progPath = absoluteFullFile pre cFile
  let uniquePath = programWithinToolDir progPath a
  copyFile progPath uniquePath

  let mountStr = outDir++":/host"
  let args = ["-k", "5", (show t), 
              "docker", 
              "run",
              "-v",
              mountStr,
              "seahorn/seahorn"]
  if debug then putStrLn $ "timeout "++(show args) else return ()
  (_, stdOut, _) <- readProcessWithExitCode "timeout" args ""
  return stdOut

runAnalyzer :: Program -> DebugMode -> Maybe Int -> FilePath -> FilePath -> DseTool -> Property -> Bool -> Bool -> Analyzer -> IO (Maybe AnalysisWitness)
runAnalyzer p d mTag logPre _ dTool prp isMinAca hasReach a@(Analyzer Seahorn _ _ _ _ _ _ _ _) = do
  let outputDir = deriveOutputDir p a mTag
  createDirectoryIfMissing True outputDir
  let cFile = sourcePath p
  let t = deriveTimeout a mTag p
  let debug = ((d == Full) || (d == Analyzers))

  start <- getCurrentTime
  rawResult <- launchSeahorn cFile a t outputDir mTag logPre debug
  end <- getCurrentTime
  let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start

  if debug
    then do putStrLn rawResult
    else do return ()

  result <- parseResult rawResult a p elapsedTime mTag True logPre dTool prp isMinAca hasReach
  return result
runAnalyzer p _ mTag logPre _ dTool prp isMinAca hasReach a@(Analyzer CIVL _ _ _ _ _ _ _ _) = do
  start <- getCurrentTime
  let t = deriveTimeout a mTag p
  rawResult <- symbolicExecution p t
  end <- getCurrentTime
  let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
  result <- parseResult rawResult a p elapsedTime mTag True logPre dTool prp isMinAca hasReach
  return result
runAnalyzer p d mTag logPre _ dTool prp isMinAca hasReach a = do
  let t = deriveTimeout a mTag p
  start <- getCurrentTime
  let debug = ((d == Full) || (d == Analyzers))
  (exitCode, rawResult, stdErr) <- (executeAnalyzer p a t mTag debug logPre prp)
  end <- getCurrentTime
  let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
  
  if debug
    then do
      putStrLn $ "STDERR:\n" ++ stdErr
      putStrLn $ "STDOUT:\n" ++ rawResult
      putStrLn $ "EXIT CODE:\n" ++ (show exitCode)
    else do return ()

  result <- parseResult rawResult a p elapsedTime mTag debug logPre dTool prp isMinAca hasReach
  if debug then do putStrLn $ "\n***\nAnalysis result for " ++ (show a) ++ ": " ++ (show result) ++ "\n***\n" else do return ()
  return result

deriveTimeout :: Analyzer -> Maybe Int -> Program -> Int
deriveTimeout a Nothing (Program _ _ _ iter _) =
  if iter == 1
    then initTimeout a
    else analysisTimeout a
deriveTimeout a _ _ = generalizeTimeout a

executeAnalyzer :: Program -> Analyzer -> Int -> Maybe Int -> Bool -> FilePath -> Property -> IO (ExitCode, String, String)
executeAnalyzer p a t mTag debug logPre prp = do
  let outputDir = deriveOutputDir p a mTag
  createDirectoryIfMissing True outputDir
  let cFile = sourcePath p
  runTool cFile a t outputDir mTag debug logPre prp

runIkos :: FilePath -> Int -> IO (ExitCode, String, String)
runIkos f _ = do
  dir <- analyzerDir
  let ikosBin = dir++"/ikos/bin/ikos"
  readProcessWithExitCode ikosBin ["-d=dbm", "-a=prover", f] ""

ikosHeader :: IO FilePath
ikosHeader = do
  dir <- analyzerDir
  let headerFile = dir++"/ikos/ikos_header"
  return headerFile

addIkosHeader :: Program -> Maybe Int -> IO FilePath
addIkosHeader p Nothing = do
  let ikosCopy = ("/tmp/"++(pName p)++".c")
  header <- ikosHeader
  callCommand $ "cat "++header++" > "++ikosCopy
  callCommand $ "cat "++(sourcePath p)++" >> "++ikosCopy
  callCommand $ "sed -i '/extern /c\\' "++ikosCopy
  return ikosCopy
addIkosHeader p (Just i) = do
  let ikosCopy = ("/tmp/"++(pName p)++(show i)++".c")
  header <- ikosHeader
  callCommand $ "echo "++header++" > "++ikosCopy
  callCommand $ "sed -i '/extern /c\\' "++ikosCopy  
  callCommand $ "cat "++(sourcePath p)++" >> "++ikosCopy
  return ikosCopy

deriveOutputDir :: Program -> Analyzer -> Maybe Int -> FilePath
deriveOutputDir p a Nothing = (iterLogPath p) ++ (show a)
deriveOutputDir p a (Just tag) = (iterLogPath p) ++ (show tag) ++ "/" ++ (show a)

executeValidator :: Program -> Analyzer -> Analyzer -> Maybe Int -> Bool -> FilePath -> Property -> IO (ExitCode, String, String)
executeValidator p validator concreteAnalyzer mTag debug logPre prp = do
  let outputDir = (deriveOutputDir p concreteAnalyzer mTag) ++ "/" ++ "validation"
  createDirectoryIfMissing True outputDir
  let timeout = 90 -- standard witness timeout
  let cFile = sourcePath p
  (exitCode, stdout, stderr) <- runValidator cFile validator timeout outputDir mTag debug logPre concreteAnalyzer prp

  if debug
  then do
    putStrLn $ "Validator STDERR:\n" ++ stderr
    putStrLn $ "Validator STDOUT:\n" ++ stdout
    putStrLn $ "Validator EXIT CODE:\n" ++ (show exitCode)
  else do return ()

  return (exitCode, stdout, stderr)

dirToValidate :: FilePath -> FilePath
dirToValidate outDir = intercalate "/" $ init $ splitOn "/" outDir

validatorSetup :: FilePath -> Analyzer -> Int -> String -> Maybe Int -> Bool -> FilePath -> Analyzer -> Property -> IO String
validatorSetup cFile a _ oDir mTag _ logPre concreteAnalyzer prp = do
  currDir <- getCurrentDirectory
  let pre = absolutePrefix logPre currDir
      outDir = absoluteOutDir pre oDir
      progPath = absoluteFullFile pre cFile
      uniquePath = programWithinToolDir progPath concreteAnalyzer
      progName = last $ splitOn "/" uniquePath
      witnessFile = (dirToValidate outDir)++"/witness.graphml"
  copyFile progPath (outDir++"/"++progName)
  copyFile witnessFile (outDir++"/witness.graphml") 
  return outDir

runValidator :: FilePath -> Analyzer -> Int -> String -> Maybe Int -> Bool -> FilePath -> Analyzer -> Property -> IO (ExitCode, String, String)
runValidator a b c d e f g _ prp = runTool a b c d e f g prp
runValidator cFile a timeout oDir mTag debug logPre concreteAnalyzer prp = error "need to implement concrete analyzer"
{-
runValidator cFile a timeout oDir mTag debug logPre concreteAnalyzer prp = do
  outDir <- validatorSetup cFile a timeout oDir mTag debug logPre concreteAnalyzer prp
  let containerName = "portfolio"
  let args = ["-k", "5", (show timeout), 
              "docker",
              "run",
              "--privileged",
              "-v",(outDir++":"++"/home/alpaca_logs"),
              "-v","/sys/fs/cgroup:/sys/fs/cgroup:rw",
              containerName]
  if debug then putStrLn ("Call to docker-benchexec:\n\n "++"timeout "++(show args)) else return ()
  readProcessWithExitCode "timeout" args ""
-}

aContainerName :: Analyzer -> String
aContainerName (Analyzer CPA_Seq _ _ _ _ _ _ _ _) = "cpa"
aContainerName (Analyzer CPA_BAM_BnB _ _ _ _ _ _ _ _) = "cpabnb"
aContainerName (Analyzer CPA_BAM_SMG _ _ _ _ _ _ _ _) = "cpasmg"
aContainerName (Analyzer UAutomizer _ _ _ _ _ _ _ _) = "ua"
aContainerName (Analyzer UKojak _ _ _ _ _ _ _ _) = "uk"
aContainerName (Analyzer UTaipan _ _ _ _ _ _ _ _) = "ut"
aContainerName (Analyzer VeriAbs _ _ _ _ _ _ _ _) = "veriabs"
aContainerName (Analyzer VeriFuzz _ _ _ _ _ _ _ _) = "verifuzz"
aContainerName (Analyzer t _ _ _ _ _ _ _ _) = error $ "have not implemented docker container for"++(show t)

runTool :: FilePath -> Analyzer -> Int -> String -> Maybe Int -> Bool -> FilePath -> Property -> IO (ExitCode, String, String)
runTool cFile a timeout oDir mTag debug logPre prp = do
  currDir <- getCurrentDirectory
  baseDir <- getAnalyzerDir
  let pre = absolutePrefix logPre currDir
  let outDir = absoluteOutDir pre oDir
  let progPath = absoluteFullFile pre cFile
  let uniquePath = programWithinToolDir progPath a
  copyNonvalidatorFile a progPath uniquePath
  -- need to get base directory of uniquePath
  let uniqParentDir = uniqueParent progPath a
  let fName = takeFileName cFile
--  let t = show $ analysisTool a
--  let containerFile = "/home/alpaca_logs/"++fName++"."++t++".c"
--  let xmlString = constructXML a containerFile baseDir True prp
--  let xmlHandle = makeXmlHandle pre oDir a mTag
--  writeFile xmlHandle xmlString
  let containerName = aContainerName a
  let args = ["-k", "5", (show timeout), 
              "docker",
              "run",
              "-v",(outDir++":"++"/alpaca_out"),
              "-v",(uniqParentDir++":"++"/alpaca_in"),	      
              containerName]
  if debug then putStrLn ("Call to alpaca-runtool:\n\n "++"timeout "++(show args)) else return ()
  readProcessWithExitCode "timeout" args ""

copyNonvalidatorFile :: Analyzer -> FilePath -> FilePath -> IO ()
copyNonvalidatorFile (Analyzer CPA_Validator _ _ _ _ _ _ _ _) _ _ = return ()
copyNonvalidatorFile _ progPath uniquePath = copyFile progPath uniquePath

absolutePrefix :: FilePath -> FilePath -> FilePath
absolutePrefix "." currDir = currDir
absolutePrefix _ _ = ""

absoluteOutDir :: FilePath -> FilePath -> FilePath
absoluteOutDir "" oDir = oDir
absoluteOutDir pre oDir = pre ++ "/" ++ oDir

absoluteFullFile :: FilePath -> FilePath -> FilePath
absoluteFullFile "" cFile = cFile
absoluteFullFile pre cFile = pre ++ "/" ++ cFile

makeXmlHandle :: FilePath -> String -> Analyzer -> Maybe Int -> String
makeXmlHandle "" oDir a Nothing = oDir ++ "/" ++ (show a) ++ ".xml"
makeXmlHandle "" oDir a (Just tag) = oDir ++ "/" ++ (show a) ++ "." ++ (show tag) ++ ".xml"
makeXmlHandle pre oDir a Nothing = pre ++ "/" ++ oDir ++ "/" ++ (show a) ++ ".xml"
makeXmlHandle pre oDir a (Just tag) = pre ++ "/" ++ oDir ++ "/" ++ (show a) ++ "." ++ (show tag) ++ ".xml"

safeResult :: Analyzer -> Float -> AnalysisWitness
safeResult a t = AnalysisWitness {
      analyzer = a,
      programPath = "",
      witnessPath = "",
      isSafe=True,
      branches=[],
      analysisTime=t,
      parsingTime=0}
               
parseResult :: String -> Analyzer -> Program -> Float -> Maybe Int -> Bool -> FilePath -> DseTool -> Property -> Bool -> Bool -> IO (Maybe AnalysisWitness)
parseResult stdout a@(Analyzer Seahorn _ _ _ _ _ _ _ _) _ time _ _ _ _ _ _ _ = do
  let lastLine = last $ lines stdout
  if "unsat" `isInfixOf` lastLine
    then return $ (Just (safeResult a time))
    else return Nothing
parseResult res a@(Analyzer CIVL _ _ _ _ _ _ _ _) p time mTag debug logPre dTool prp _ _ = do
  let summary = getCivlResultSummary res
  case summary of
    FalseResult -> parseWitness a p time mTag debug logPre dTool prp
    _ -> do return Nothing
parseResult res a p time mTag debug logPre dTool prp False hasReach = do
  let path = deriveOutputDir p a mTag
  let summary = getResultSummary res a
  case summary of
    TrueResult -> do
      if hasReach && ((iteration p)==1)
        then return Nothing
        else do
          _ <- tryToGatherWitness path
          removeArtifacts path
          return $ validateResult a time
    FalseResult -> parseWitness a p time mTag debug logPre dTool prp
    _ -> do removeArtifacts path; return Nothing
-- this case is for Vicuna
parseResult res a p time mTag debug logPre dTool prp True _ = do
  let path = deriveOutputDir p a mTag
  let summary = getResultSummary res a
  _ <- tryToGatherWitness path -- for Vicuna
  let r = vicunaData summary a prp
  putStrLn $ show r
  case summary of 
    UnknownResult -> return Nothing
    _ -> do exitImmediately ExitSuccess; return Nothing

vicunaData :: AnalysisResult -> Analyzer -> Property -> VicunaData
vicunaData res a prp = VicunaData res a prp

data VicunaData = VicunaData AnalysisResult Analyzer Property deriving (Show)

getCivlResultSummary :: String -> AnalysisResult
getCivlResultSummary output =
  let falseFound = isInfixOf "The program MAY NOT be correct. "
  in
    if falseFound output
      then FalseResult
      else UnknownResult

validateResult :: Analyzer -> Float -> Maybe AnalysisWitness
validateResult a t
  | safeOverapproximation a = Just (safeResult a t)
  | otherwise               = Nothing

{- Taken from rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell-}
search :: System.FilePath.GlobPattern.GlobPattern -> FilePath -> IO [FilePath]
search pat dir = System.FilePath.Find.find always (fileName ~~? pat) dir

removeDoubleSlash :: FilePath -> FilePath
removeDoubleSlash d = replace "//" "/" d

tryToGatherWitness :: FilePath -> IO (Maybe FilePath)
tryToGatherWitness dir = do
  let dir' = removeDoubleSlash dir
  files <- search "*.graphml" dir'
  if null files
    then do return Nothing
    else do
      let originalHandle = head files
      fileExists <- doesFileExist originalHandle
      if fileExists
        then do
          let newHandle = dir' ++ "/witness.graphml"
          renameFile originalHandle newHandle
          return (Just newHandle)
        else do return Nothing

normalizedWitnessName :: FilePath -> FilePath -> FilePath
normalizedWitnessName pre w = pre ++ "/" ++ w

normalizeWitness :: Maybe FilePath -> Program -> Analyzer -> Maybe Int -> Bool -> FilePath -> DseTool -> Property -> IO (Maybe FilePath)
normalizeWitness Nothing _ _ _ _ _ _ _ = return Nothing
normalizeWitness f _ (Analyzer _ _ _ _ _ _ BranchDirectives _ _) _ _ _ _ _ = return f
normalizeWitness f _ _ _ _ _ CpaSymExec _ = return f
normalizeWitness (Just w) p a mTag debug logPre _ prp = do
  currDir <- getCurrentDirectory
  let pre = absolutePrefix logPre currDir
  let witness = pre ++ "/" ++ w
  witnessValidator <- cpaValidator witness (normalizedWitnessName pre w)
  _ <- executeValidator p witnessValidator a mTag debug logPre prp
  let validatorDir = pre ++ "/" ++ (deriveOutputDir p a mTag) ++ "/validation"
  tryToGatherWitness validatorDir

transformWrapAround :: FilePath -> IO ()
transformWrapAround w = do
  {- If a witness contains string "\\result==2147483648",
     we transform it to "\\result==-2147483648" in-place.
     These two numbers are equivalent in a 32-bit model, however
     CPA's witness validator incorrectly interprets the first
     number as being positive, so we put some scotch on this.
  -}
  callProcess "/usr/bin/sed" ["-i","s/==2147483648/==-2147483648/",w]

cpaValidator :: FilePath -> FilePath -> IO Analyzer
cpaValidator witness witnessXmlName = do
  transformWrapAround witness
  portfolioDir <- getPortfolioDir
  return $ Analyzer {
    analysisTool = CPA_Validator,
    analysisName = "cpachecker",
    analysisDir = portfolioDir ++ "CPA_Seq",
    analysisOptions = [
      ("-witnessValidation", Nothing),
      ("-witness", Just witnessXmlName),
      ("-setprop", Just "cfa.allowBranchSwapping=false"),
      ("-setprop", Just "cpa.arg.witness.exportSourcecode=true")],
    safeOverapproximation = True,
    analysisTimeout = 90, -- standard timeout for witness validation
    witnessType = BranchDirectives,
    generalizeTimeout = 90,
    initTimeout = 90
    }

removeArtifacts :: FilePath -> IO ()
removeArtifacts path = do
  let pat1 = "*.files"
  let pat2 = "validation/*.files"
  dir1 <- globDir1 (compile pat1) path
  dir2 <- globDir1 (compile pat2) path
  mapM_ removeDirectoryRecursiveIfExists dir1
  mapM_ removeDirectoryRecursiveIfExists dir2

removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists dir = do
  dirExists <- doesDirectoryExist dir
  if dirExists
    then do removeDirectoryRecursive dir
    else do return ()
    
parseWitness :: Analyzer -> Program -> Float -> Maybe Int -> Bool -> FilePath -> DseTool -> Property -> IO (Maybe AnalysisWitness)
parseWitness a p time mTag debug logPre CpaSymExec prp = do
  let pathPrefix = deriveOutputDir p a mTag
  start <- getCurrentTime
  w <- tryToGatherWitness pathPrefix
  witness <- normalizeWitness w p a mTag debug logPre CpaSymExec prp
  removeArtifacts pathPrefix
  if isJust witness
    then do
      let (Just witnessP) = witness
      let a' = adaptiveTimeCalculation a time
      end <- getCurrentTime
      let pTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
         
      return (Just AnalysisWitness {
                 analyzer=a',
                 programPath=(sourcePath p),
                 witnessPath=witnessP,
                 isSafe=False,
                 branches=[],
                 analysisTime=time,
                 parsingTime=pTime}
                 )
    else return Nothing
parseWitness a p time mTag debug logPre dTool prp = do
  let pathPrefix = deriveOutputDir p a mTag
  start <- getCurrentTime
  w <- tryToGatherWitness pathPrefix
  witness <- normalizeWitness w p a mTag debug logPre dTool prp
  removeArtifacts pathPrefix
  if isJust witness
    then do
      let (Just witnessP) = witness
      let containsSource = witnessContainsSource a
      maybeB <- getBranches containsSource witnessP
      if isNothing maybeB
        then return Nothing
        else do
          let (Just b) = maybeB
          if null b
            then return Nothing
            else do
              let b' = map ensureDefaultTrue b
              let a' = adaptiveTimeCalculation a time
              end <- getCurrentTime
              let pTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
         
              return (Just AnalysisWitness {
                         analyzer=a',
                         programPath=(sourcePath p),
                         witnessPath=witnessP,
                         isSafe=False,
                         branches=b',
                         analysisTime=time,
                         parsingTime=pTime}
                     )
    else return Nothing

ensureDefaultTrue :: Branch -> Branch
ensureDefaultTrue (Branch l _ s@(Just "default:")) = Branch l True s
ensureDefaultTrue b = b

witnessContainsSource :: Analyzer -> Bool
witnessContainsSource (Analyzer t _ _ _ _ _ _ _ _) = toolWritesSourceInWitness t

toolWritesSourceInWitness :: AnalysisTool -> Bool
toolWritesSourceInWitness CPA_Seq = True
toolWritesSourceInWitness CPA_BAM_BnB = True
toolWritesSourceInWitness InterpChecker = True
toolWritesSourceInWitness _ = False

adaptiveTimeCalculation :: Analyzer -> Float -> Analyzer
adaptiveTimeCalculation a newTime =
  if (round newTime) < (analysisTimeout a)
    then a { analysisTimeout = (round newTime) + 100 }
    else a

cpaSymExecSetup :: AnalysisWitness -> IO (FilePath, [String], FilePath)
cpaSymExecSetup w = do
  a <- getAnalyzerDir
  let cpaDir = a++"cpa_symexec/cpachecker/"
  let cpaScript = cpaDir++"scripts/cpa.sh"
  let valConfig = "witness.validation.violation.config="++cpaDir++"config/violation-witness-validation-symexec.properties"
  let specFile = cpaDir++"config/specification/sv-comp-reachability.spc"
  let outputDir = (takeDirectory (witnessPath w)) ++ "/symexec_output"
  let args = ["-witnessValidation","-setprop",valConfig,"-witness",(witnessPath w),"-spec",specFile,"-outputpath",outputDir,(programPath w)]
  return (cpaScript, args, outputDir)

guidedSymExec :: AnalysisWitness -> DseTool -> DebugMode -> Bool -> IO (Maybe SymExecResult)
guidedSymExec w CpaSymExec d _ = do
  (cpaScript, args, outDir) <- cpaSymExecSetup w
  (_,stdout,stderr) <- readProcessWithExitCode cpaScript args ""
  
  let debug = ((d == Full) || (d == Direct))
  if debug
    then do
      let fullCmd = [cpaScript]++args
      putStrLn $ "SYMEXEC COMMAND:"++(unwords fullCmd)++"\n"
      putStrLn $ "STDOUT:"++stdout
      putStrLn $ "STDERR:"++stderr
    else return ()

  pc <- search "*.symbolic-trace.txt" outDir
  if null pc
    then return Nothing
    else do
      let pcFile = head pc
      return $ Just $ CpaResult pcFile
guidedSymExec w CivlSymExec d valid = do
  let progPath = programPath w
  directPath <- makeDirectiveFile w
  r <- runDirectedCivl (progPath, directPath) d valid
  if (emptyCivlOutput r)
    then return Nothing
    else return (Just CivlResult {resultPath=progPath,resultString=r})

emptyCivlOutput :: String -> Bool
emptyCivlOutput "" = True
emptyCivlOutput output =
  if (isInfixOf "Time out." output)
    then True
    else not $ isInfixOf "=== Result ===" output

getCivlJar :: IO String
getCivlJar = do
  aDir <- getAnalyzerDir
  let jarFile = aDir ++ "civl/civl.jar"
  return jarFile

getCivlDir :: IO String
getCivlDir = do
  aDir <- getAnalyzerDir
  return $ aDir ++ "civl"

directedDebug :: FilePath -> (ExitCode, String, String) -> DebugMode -> Bool -> IO ()
directedDebug directFile (_, stdOut, stdErr) d valid = do
  let debug = ((d == Full) || (d == Direct))
  if debug
    then do
      directFile' <- makeAbsolute directFile
      if valid
        then putStrLn $ "Contents of valid directive file: " ++ directFile' ++ "\n"
        else putStrLn $ "Contents of directive file: " ++ directFile' ++ "\n"
      fileStr <- readFile directFile'; putStrLn fileStr
      putStrLn "\nDirected CIVL output:\n"; putStrLn stdOut
      putStrLn "\nDirected CIVL stderr:\n"; putStrLn stdErr
    else do return ()

symbolicExecution :: Program -> Int -> IO String
symbolicExecution (Program uniquePath _ _ _ _) t = do
  civlJar <- getCivlJar
  program' <- makeAbsolute uniquePath
  let args = ["-jar", civlJar, "verify", "-svcomp17", ("-timeout="++(show t)), program']
  (_, stdOut, _) <- readProcessWithExitCode "java" args ""
  return stdOut

directedCivlArgs :: FilePath -> FilePath -> Bool -> IO [String]
directedCivlArgs program directFile valid = do
  civlJar <- getCivlJar
  putStrLn $ "************** the CIVL JAR IS: "++civlJar
  directFile' <- makeAbsolute directFile
  let directArg = "-direct="++directFile'
  program' <- makeAbsolute program
  if valid
    then return $ ["-jar", civlJar, "verify", "-validPath", "-timeout=120", "-svcomp17", directArg, program']
    else return $ ["-jar", civlJar, "verify", "-svcomp17", "-timeout=120", directArg, program']

sliceCivlArgs :: FilePath -> Bool -> IO [String]
sliceCivlArgs program valid = do
  civlJar <- getCivlJar
  program' <- makeAbsolute program
  if valid
    then return $ ["-jar", civlJar, "replay", "-validPath", "-sliceAnalysis", "-timeout=120", program']
    else return $ ["-jar", civlJar, "replay", "-sliceAnalysis", "-timeout=120", program']

runDirectedCivl :: (FilePath, FilePath) -> DebugMode -> Bool -> IO String
runDirectedCivl (program, directFile) d valid = do
  putStrLn "running directed civl"
  args <- directedCivlArgs program directFile valid
  putStrLn "** ## args:"
  putStrLn $ concat $ intersperse " " $ args
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode "java" args ""
  putStrLn "stdout"
  putStrLn stdOut
  directedDebug directFile (exitCode, stdOut, stdErr) d valid
  return stdOut

runSliceAnalysis :: FilePath -> DebugMode -> Bool -> IO String
runSliceAnalysis program d valid = do
  args <- sliceCivlArgs program valid
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode "java" args ""
  sliceDebug (exitCode, stdOut, stdErr) d valid
  return stdOut

sliceDebug :: (ExitCode, String, String) -> DebugMode -> Bool -> IO ()
sliceDebug (_, stdOut, stdErr) d valid = do
  let debug = ((d == Full) || (d == Slice))
  if debug
    then do
      if valid
        then putStrLn "Valid CIVL slice output:\n"
        else putStrLn "CIVL slice output:"
      putStrLn stdOut; putStrLn ""
      if valid
        then putStrLn "Valid CIVL slice stderr:"
        else putStrLn "CIVL slice stderr:"
      putStrLn stdErr; putStrLn ""
    else do return ()

makeDirectiveFile :: AnalysisWitness -> IO FilePath
makeDirectiveFile w = do
  let directivePath = (dropExtension $ programPath w)
                      ++ "." ++ (show (analyzer w))
                      ++ ".direct"
  w' <- removeUnnecessaryBranches w                      
  writeFile directivePath (makeWitnessString w')
  return directivePath

-- CPA writes branch directive for __VERIFIER_assume(), which
-- messes up the directives in CIVL; removing them here is hackish
removeUnnecessaryBranches :: AnalysisWitness -> IO AnalysisWitness
removeUnnecessaryBranches (AnalysisWitness a pp wp s bs t pt) = do
  let badBranches = filter isIllegitimateBranch bs
  let badLines = map line badBranches
  let bs' = filter (\b -> ((line b) `notElem` badLines)) bs
  return (AnalysisWitness a pp wp s bs' t pt)

isIllegitimateBranch :: Branch -> Bool
isIllegitimateBranch (Branch _ _ Nothing) = False
isIllegitimateBranch (Branch _ _ (Just s)) = "aca_input_arr" `isInfixOf` s

makeWitnessString :: AnalysisWitness -> String
makeWitnessString w = programName w ++ "\n\
  \lines " ++ (intercalate " " $ map (show . line) (branches w)) ++ "\n\
  \guide " ++ (intercalate " " $ map (boolToIntStr . guide) (branches w)) ++ "\n"

programName :: AnalysisWitness -> String
programName w = last $ splitOn "/" $ programPath w

boolToIntStr :: Bool -> String
boolToIntStr True = "1"
boolToIntStr False = "0"

data SymExecResult = CivlResult {
    resultPath :: FilePath,
    resultString :: String }
  |
  CpaResult {
    pathToPc :: FilePath  
  } deriving (Show)
  
confirmedFailure :: SymExecResult -> Bool
confirmedFailure r@(CivlResult _ _) =
  "__VERIFIER_error at svcomp.cvl" `isInfixOf` (resultString r)
confirmedFailure (CpaResult _) = error "confirmedFailure not implemented for CpaResult"

filterScript :: IO FilePath
filterScript = do
  a <- analyzerDir
  return $ a++"/cpa_symexec/filter_sym_lines.sh"

failureSubspace :: SymExecResult -> DebugMode -> IO (Maybe Subspace)
failureSubspace (CpaResult symFile) d = do
  let debug = ((d == Full) || (d == Direct))  
  fScript <- filterScript
  (_,ls,_) <- readProcessWithExitCode fScript [symFile] ""
  let validLines = filter isSymbolicExpr (lines ls)
  let validLines' = map init validLines -- drop semicolon
  mSubspace <- extractCpaSubspace validLines'
  if (isJust mSubspace)
    then do
      let (Just subspace) = mSubspace
      if debug then do putStrLn (show subspace) else do return ()
      return (Just subspace)
    else return Nothing
failureSubspace r@(CivlResult _ _) d = do
  let debug = ((d == Full) || (d == Slice))
  s <- runSliceAnalysis (resultPath r) d False
  if debug then do putStrLn s else do return ()
  mSubspace <- extractSubspace s
  if (isJust mSubspace)
    then do
      let (Just subspace) = mSubspace
      if debug then do putStrLn (show subspace) else do return ()
      return (Just subspace)
    else return Nothing

validSubspace :: SymExecResult -> DebugMode -> IO (Maybe Subspace)
validSubspace (CpaResult _) _ = error "CpaSymExec is not currently able to block valid subspaces; try CIVL."
validSubspace r@(CivlResult _ _) d = do
  let debug = ((d == Full) || (d == Slice))
  s <- runSliceAnalysis (resultPath r) d True
  if debug then do putStrLn s else do return ()
  mSubspace <- extractSubspace s
  if (isJust mSubspace)
    then do
      let (Just subspace) = mSubspace
      if debug then do putStrLn (show subspace) else do return ()
      return (Just subspace)
    else return Nothing

extractSubspace :: String -> IO (Maybe Subspace)
extractSubspace s = do
  if ("Time out." `isInfixOf` s)
    then return Nothing
    else do
      let preS = parseCivlOutput $ reduceCivlOutput s
      ss <- makeSubspace preS
      return (Just ss)

makeSubspace :: PreSubspace -> IO Subspace
makeSubspace (PreSubspace (RawConjunction rc n) (RawConjunction ac _) cMap tMap) = do
  cs <- mapM (\(RawConjunct s)->parseToExpr s) rc
  as <- mapM (\(RawConjunct s)->parseToExpr s) ac
  let cs' = map Conjunct cs
  let as' = map Conjunct as
  return $ Subspace cs' as' cMap tMap n

type LineNumber = String

checkResults :: [AnalysisWitness] -> DseTool -> DebugMode -> Bool -> FilePath -> IO EvidenceCollection
checkResults rs dTool d blockV exitFile = do
  cs <- mapConcurrently (checkAnalysisWitness dTool d blockV exitFile) rs
  let debug = (d == Full)
  if debug then do mapM_ (putStrLn . show) cs else do return ()
  let cs' = filter (not . isEmptyEvidence) cs
  return cs'

isEmptyEvidence :: PieceOfEvidence -> Bool
isEmptyEvidence (EmptyEvidence _) = True
isEmptyEvidence _ = False

programWithinToolDir :: FilePath -> Analyzer -> String
programWithinToolDir p a =
  let prefix = concat $ intersperse "/" $ init $ splitOn "/" p
      progName = last $ splitOn "/" p
      programInDir = prefix ++ "/" ++ (show a) ++ "/" ++ progName 
  in programInDir ++ "." ++ (show a) ++ ".c"

uniqueParent :: FilePath -> Analyzer -> String
uniqueParent p a =
  let prefix = concat $ intersperse "/" $ init $ splitOn "/" p
      progName = last $ splitOn "/" p
      programInDir = prefix ++ "/" ++ (show a) ++ "/" 
  in programInDir

checkAnalysisWitness :: DseTool -> DebugMode -> Bool -> FilePath -> AnalysisWitness -> IO PieceOfEvidence
checkAnalysisWitness _ _ _ _ w@(AnalysisWitness _ _ _ True _ _ _) = return (LegitimateEvidence w emptySubspace 0)
checkAnalysisWitness CpaSymExec d _ _ witness@(AnalysisWitness _ _ wPath _ _ _ _) = do
  let witness' = witness
  -- the following hack is to remove docker absolute paths written by Ultimate* tools
  -- that cause CPA-SymExec to fail when replaying their witness
  callCommand $ "sed -i '/<data key=\"originfile\">/d' "++wPath
  result <- (guidedSymExec witness' CpaSymExec d False)
  if isJust result
    then do
      let Just symExecResult = result
      start <- getCurrentTime        
      mFailSpace <- failureSubspace symExecResult d
      end <- getCurrentTime
      let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
          
      if (isJust mFailSpace)
        then do
          let (Just failSpace) = mFailSpace
          return $ (LegitimateEvidence witness failSpace elapsedTime)
        else return (EmptyEvidence witness')
    else return (EmptyEvidence witness')
checkAnalysisWitness CivlSymExec d blockV _ witness@(AnalysisWitness tool progPath _ _ _ _ _) = do
  -- a result contains:
  --  a file handle (passed by the witness) and
  --  the output from CIVL's run
  let uniquePath = programWithinToolDir progPath tool
  let witness' = witness { programPath = uniquePath }
  result <- (guidedSymExec witness' CivlSymExec d False)
  if isJust result
    then let Just symExecResult = result
    in
      if confirmedFailure symExecResult
        then do
          start <- getCurrentTime        
          mFailSpace <- failureSubspace symExecResult d
          end <- getCurrentTime
          let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
          
          if (isJust mFailSpace)
            then do
              let (Just failSpace) = mFailSpace
              return $ (LegitimateEvidence witness failSpace elapsedTime)
            else return (EmptyEvidence witness')
        else do
          if blockV
            then do
              start <- getCurrentTime                    
              maybeValidSpace <- (guidedSymExec witness' CivlSymExec d True)
              end <- getCurrentTime
              let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
              
              if isJust maybeValidSpace
                then do
                  let Just validResult = maybeValidSpace
                  mValidSpace <- validSubspace validResult d
                  if (isJust mValidSpace)
                    then do
                      let (Just validSpace) = mValidSpace
                      return (SpuriousEvidence witness validSpace elapsedTime)
                    else return (EmptyEvidence witness')
                else do
                  return (SpuriousEvidence emptyWitness emptySubspace elapsedTime)
            else return (SpuriousEvidence emptyWitness emptySubspace (-1))
    else return (EmptyEvidence witness')

mergeEvidence :: EvidenceCollection -> EvidenceCollection
mergeEvidence = nubBy equalEvidence

equalEvidence :: PieceOfEvidence -> PieceOfEvidence -> Bool
equalEvidence (SpuriousEvidence _ _ _) _ = False
equalEvidence _ (SpuriousEvidence _ _ _) = False
equalEvidence w1 w2 = (characterization w1)==(characterization w2)

emptySubspace :: Subspace
emptySubspace = Subspace {
  conjunction=[],
  sAssumptions=[],
  inputIdCounts=(Map.fromList []),
  inputIdTypes=(Map.fromList []),
  nSlice=0
  }
\end{code}
