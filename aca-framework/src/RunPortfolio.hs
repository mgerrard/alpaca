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
import Data.Maybe
import Data.List
import Data.Time
import CscTypes
import Portfolio
import Branch
import CivlParsing
import LocalPaths
import AcaComputation
import LaunchBenchexec
import Reading

type Thread = Async (Maybe AnalysisWitness)

runPortfolio :: Program -> Portfolio -> Csc -> RunConfiguration -> IO EvidenceCollection
runPortfolio prog pfolio csc (RunConfiguration InParallel d tag exitStrat bValid logPre exitFile) = do
  {- update log : track total analysis runtime -}
  threads <- launchPortfolio prog d tag logPre exitFile pfolio
  results <- pollUntilDone threads pfolio exitStrat csc d bValid exitFile
  mapM_ cancel threads
  return results
runPortfolio prog pfolio csc (RunConfiguration InSequence d tag _ bValid logPre exitFile) = do
  maybeWitnesses <- mapM (runAnalyzer prog d tag logPre exitFile) pfolio
  let witnesses = catMaybes maybeWitnesses
  maybeEvidence <- mapM (verifyWitness csc d bValid exitFile) witnesses
  let results = catMaybes maybeEvidence
  return results

verifyWitness :: Csc -> DebugMode -> Bool -> FilePath -> AnalysisWitness -> IO (Maybe PieceOfEvidence)
verifyWitness csc debug bValid exitFile witness = do
  if isUnreachableResult witness
    then return (Just (extractUnreachableResult witness))
    else do
      newlyClassified <- checkAnalysisWitness debug bValid exitFile witness
      let newEvidence = isNewEvidence csc newlyClassified
      if newEvidence
        then return $ (Just newlyClassified)
        else return $ Nothing
        
launchPortfolio :: Program -> DebugMode -> Maybe Int -> FilePath -> FilePath -> Portfolio -> IO [Thread]
launchPortfolio prog d tag logPre ef pfolio  = mapM (async . (runAnalyzer prog d tag logPre ef)) pfolio

pollUntilDone :: [Thread] -> Portfolio -> ExitStrategy -> Csc -> DebugMode -> Bool -> FilePath -> IO EvidenceCollection
pollUntilDone threads _ strategy csc debug bValid exitFile = do
  let finished = []
  evidence <- pollLoop threads finished strategy csc debug bValid exitFile
  return evidence

pollLoop :: [Thread] -> EvidenceCollection -> ExitStrategy -> Csc -> DebugMode -> Bool -> FilePath -> IO EvidenceCollection
pollLoop [] finished _ _ _ _ _ = return finished
pollLoop runningThreads finished exitStrat csc debug bValid ef = do
  maybeResults <- mapM poll runningThreads
  if any isUnreachableThreadResult maybeResults
    then do
      let result = extractUnreachableThreadResult maybeResults
      return [result]
    else if any isJust maybeResults
      then do
        let (running', stopped) = partitionRunningAndFinished runningThreads maybeResults
        newlyClassified <- (checkResults stopped debug bValid ef)
--        let finished' = finished ++ newlyClassified
        let newEvidence = filter (isNewEvidence csc) newlyClassified
        let finished' = finished ++ newEvidence
        if ((exitStrat == Eager) && (any isNonspuriousEvidence newEvidence))
          then return finished' 
          else pollLoop running' finished' exitStrat csc debug bValid ef
      else do
        threadDelay 100000 -- poll every 0.1 seconds
        pollLoop runningThreads finished exitStrat csc debug bValid ef

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
isUnreachableThreadResult (Just (Right (Just (AnalysisWitness _ _ True _ _ _)))) = True
isUnreachableThreadResult _ = False

isUnreachableResult :: AnalysisWitness -> Bool
isUnreachableResult (AnalysisWitness _ _ True _ _ _) = True
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

getSeahornBin :: IO String
getSeahornBin = return "/home/mitch/work/seahorn/build/run/bin/sea"

launchSeahorn :: Program -> Int -> IO String
launchSeahorn (Program uniquePath _ _ _ _) t = do
  seahornBin <- getSeahornBin
  program' <- makeAbsolute uniquePath
  let args = [(show t), seahornBin, "pf", program']
  (_, stdOut, _) <- readProcessWithExitCode "timeout" args ""
  return stdOut

runAnalyzer :: Program -> DebugMode -> Maybe Int -> FilePath -> FilePath -> Analyzer -> IO (Maybe AnalysisWitness)
runAnalyzer p _ mTag logPre _ a@(Analyzer Seahorn _ _ _ _ _ _ _ _) = do
  start <- getCurrentTime
  let t = deriveTimeout a mTag p
  rawResult <- launchSeahorn p t
  end <- getCurrentTime
  let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
  result <- parseResult rawResult a p elapsedTime mTag True logPre
  return result
runAnalyzer p _ mTag logPre _ a@(Analyzer CIVL _ _ _ _ _ _ _ _) = do
  start <- getCurrentTime
  let t = deriveTimeout a mTag p
  rawResult <- symbolicExecution p t
  end <- getCurrentTime
  let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
  result <- parseResult rawResult a p elapsedTime mTag True logPre
  return result
runAnalyzer p d mTag logPre _ a = do
  let t = deriveTimeout a mTag p
  start <- getCurrentTime
  let debug = ((d == Full) || (d == Analyzers))
  (exitCode, rawResult, stdErr) <- (executeAnalyzer p a t mTag debug logPre)
  end <- getCurrentTime
  let elapsedTime = (realToFrac :: (Real a) => a -> Float) $ diffUTCTime end start
  
  if debug
    then do
      putStrLn $ "STDERR:\n" ++ stdErr
      putStrLn $ "STDOUT:\n" ++ rawResult
      putStrLn $ "EXIT CODE:\n" ++ (show exitCode)
    else do return ()

  result <- parseResult rawResult a p elapsedTime mTag debug logPre
  if debug then do putStrLn $ "\n***\nAnalysis result for " ++ (show a) ++ ": " ++ (show result) ++ "\n***\n" else do return ()
  return result

deriveTimeout :: Analyzer -> Maybe Int -> Program -> Int
deriveTimeout a Nothing (Program _ _ _ iter _) =
  if iter == 1
    then initTimeout a
    else analysisTimeout a
deriveTimeout a _ _ = generalizeTimeout a

executeAnalyzer :: Program -> Analyzer -> Int -> Maybe Int -> Bool -> FilePath -> IO (ExitCode, String, String)
executeAnalyzer p a t mTag debug logPre = do
  let outputDir = deriveOutputDir p a mTag
  createDirectoryIfMissing True outputDir
  let cFile = sourcePath p
  runBenchexec cFile a t outputDir mTag debug logPre

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

executeValidator :: Program -> Analyzer -> Analyzer -> Maybe Int -> Bool -> FilePath -> IO (ExitCode, String, String)
executeValidator p validator concreteAnalyzer mTag debug logPre = do
  let outputDir = (deriveOutputDir p concreteAnalyzer mTag) ++ "/" ++ "validation"
  createDirectoryIfMissing True outputDir
  let timeout = 90 -- standard witness timeout
  let cFile = sourcePath p
  (exitCode, stdout, stderr) <- runBenchexec cFile validator timeout outputDir mTag debug logPre

  if debug
  then do
    putStrLn $ "Validator STDERR:\n" ++ stderr
    putStrLn $ "Validator STDOUT:\n" ++ stdout
    putStrLn $ "Validator EXIT CODE:\n" ++ (show exitCode)
  else do return ()

  return (exitCode, stdout, stderr)
  
runBenchexec :: FilePath -> Analyzer -> Int -> String -> Maybe Int -> Bool -> FilePath -> IO (ExitCode, String, String)
runBenchexec cFile a timeout oDir mTag debug logPre = do
  currDir <- getCurrentDirectory
  baseDir <- getAnalyzerDir
  let aDir = analysisDir a
  let pre = absolutePrefix logPre currDir
  let outDir = absoluteOutDir pre oDir
  let fullFile = absoluteFullFile pre cFile
  let xmlString = constructXML a fullFile baseDir
  let xmlHandle = makeXmlHandle pre oDir a mTag
  writeFile xmlHandle xmlString
  script <- benchexecScript
  let args = [script, cFile, xmlHandle, outDir, aDir, (show timeout)]
  if debug then putStrLn ("Call to benchexec:\n\n "++"bash "++(show args)) else return ()
  readProcessWithExitCode "bash" args ""

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

benchexecScript :: IO String
benchexecScript = do
  dir <- analyzerDir
  return $ dir ++ "/run_benchexec.sh"

safeResult :: Analyzer -> Float -> AnalysisWitness
safeResult a t = AnalysisWitness {
      analyzer = a,
      programPath = "",
      isSafe=True,
      branches=[],
      analysisTime=t,
      parsingTime=0}
               
parseResult :: String -> Analyzer -> Program -> Float -> Maybe Int -> Bool -> FilePath -> IO (Maybe AnalysisWitness)
parseResult stdout a@(Analyzer Seahorn _ _ _ _ _ _ _ _) _ time _ _ _ = do
  let lastLine = last $ lines stdout
  if "unsat" `isInfixOf` lastLine
    then return $ (Just (safeResult a time))
    else return Nothing
parseResult res a@(Analyzer CIVL _ _ _ _ _ _ _ _) p time mTag debug logPre = do
  let summary = getCivlResultSummary res
  case summary of
    FalseResult -> parseWitness a p time mTag debug logPre
    _ -> do return Nothing
parseResult res a p time mTag debug logPre = do
  let path = deriveOutputDir p a mTag
  let summary = getResultSummary res
  case summary of
    TrueResult -> do removeArtifacts path; return $ validateResult a time
    FalseResult -> parseWitness a p time mTag debug logPre
    _ -> do removeArtifacts path; return Nothing

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

normalizeWitness :: Maybe FilePath -> Program -> Analyzer -> Maybe Int -> Bool -> FilePath -> IO (Maybe FilePath)
normalizeWitness Nothing _ _ _ _ _ = return Nothing
normalizeWitness f _ (Analyzer _ _ _ _ _ _ BranchDirectives _ _) _ _ _ = return f
normalizeWitness (Just w) p a mTag debug logPre = do
  currDir <- getCurrentDirectory
  let pre = absolutePrefix logPre currDir
  let witness = pre ++ "/" ++ w
  witnessValidator <- cpaValidator witness
  _ <- executeValidator p witnessValidator a mTag debug logPre
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

cpaValidator :: FilePath -> IO Analyzer
cpaValidator witness = do
  transformWrapAround witness
  portfolioDir <- getPortfolioDir
  return $ Analyzer {
    analysisTool = CPA_Validator,
    analysisName = "cpachecker",
    analysisDir = portfolioDir ++ "CPA_Seq",
    analysisOptions = [
      ("-witnessValidation", Nothing),
      ("-witness", Just witness),
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
    
parseWitness :: Analyzer -> Program -> Float -> Maybe Int -> Bool -> FilePath -> IO (Maybe AnalysisWitness)
parseWitness a@(Analyzer CIVL _ _ _ _ _ _ _ _) p time _ _ _ = do
  return (Just AnalysisWitness {
             analyzer=a,
             programPath=(sourcePath p),
             isSafe=False,
             branches=[],
             analysisTime=time,
             parsingTime=0
             }
         )
parseWitness a p time mTag debug logPre = do
  let pathPrefix = deriveOutputDir p a mTag
  start <- getCurrentTime
  w <- tryToGatherWitness pathPrefix
  witness <- normalizeWitness w p a mTag debug logPre
  removeArtifacts pathPrefix
  if isJust witness
    then do
      let (Just witnessPath) = witness
      let containsSource = witnessContainsSource a
      maybeB <- getBranches containsSource witnessPath
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
toolWritesSourceInWitness CPA_BAM_Slicing = True
toolWritesSourceInWitness CPA_Seq_16 = True
toolWritesSourceInWitness InterpChecker = True
toolWritesSourceInWitness _ = False

adaptiveTimeCalculation :: Analyzer -> Float -> Analyzer
adaptiveTimeCalculation a newTime =
  if (round newTime) < (analysisTimeout a)
    then a { analysisTimeout = (round newTime) + 100 }
    else a

guidedSymExe :: AnalysisWitness -> DebugMode -> Bool -> IO (Maybe CivlResult)
guidedSymExe w d valid = do
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
  args <- directedCivlArgs program directFile valid
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode "java" args ""
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
removeUnnecessaryBranches (AnalysisWitness a pp s bs t pt) = do
  let badBranches = filter isIllegitimateBranch bs
  let badLines = map line badBranches
  let bs' = filter (\b -> ((line b) `notElem` badLines)) bs
  return (AnalysisWitness a pp s bs' t pt)

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

data CivlResult = CivlResult {
  resultPath :: FilePath,
  resultString :: String
  } deriving (Show)

confirmedFailure :: CivlResult -> Bool
confirmedFailure r =
  "__VERIFIER_error at svcomp.cvl" `isInfixOf` (resultString r)

failureSubspace :: CivlResult -> DebugMode -> IO (Maybe Subspace)
failureSubspace r d = do
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

validSubspace :: CivlResult -> DebugMode -> IO (Maybe Subspace)
validSubspace r d = do
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
makeSubspace (PreSubspace rc ac cMap tMap) = do
  cs <- mapM (\(RawConjunct s)->parseToExpr s) rc
  as <- mapM (\(RawConjunct s)->parseToExpr s) ac
  let cs' = map Conjunct cs
  let as' = map Conjunct as
  return $ Subspace cs' as' cMap tMap

type SymVar = String
type LineNumber = String

checkResults :: [AnalysisWitness] -> DebugMode -> Bool -> FilePath -> IO EvidenceCollection
checkResults rs d blockV exitFile = do
  cs <- mapConcurrently (checkAnalysisWitness d blockV exitFile) rs
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
      
checkAnalysisWitness :: DebugMode -> Bool -> FilePath -> AnalysisWitness -> IO PieceOfEvidence
checkAnalysisWitness _ _ _ w@(AnalysisWitness _ _ True _ _ _) = return (LegitimateEvidence w emptySubspace 0)
checkAnalysisWitness d _ _ (AnalysisWitness (Analyzer CIVL _ _ _ _ _ _ _ _) progPath _ _ _ _) = do
  s <- runSliceAnalysis progPath d False
  mSubspace <- extractSubspace s
  if (isJust mSubspace)
    then do
      let (Just subspace) = mSubspace
      return $ (LegitimateEvidence civlWitness subspace 0)
    else return (EmptyEvidence emptyWitness)
checkAnalysisWitness d blockV _ witness@(AnalysisWitness tool progPath _ _ _ _) = do
  -- a result contains:
  --  a file handle (passed by the witness) and
  --  the output from CIVL's run
  {- Need to copy original C file and tag it with Analyzer providing
     the witness; otherwise there will be a race condition writing to
     the trace file -}
  let uniquePath = programWithinToolDir progPath tool
  copyFile progPath uniquePath
  let witness' = witness { programPath = uniquePath }
  result <- (guidedSymExe witness' d False)
  if isJust result
    then let Just civlResult = result in
      if confirmedFailure civlResult
        then do
          start <- getCurrentTime        
          mFailSpace <- failureSubspace civlResult d
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
              maybeValidSpace <- (guidedSymExe witness' d True)
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
  inputIdTypes=(Map.fromList [])
  }
