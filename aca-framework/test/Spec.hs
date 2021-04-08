import Test.Hspec
import CscTypes
import Analysis
import Writing
import Configuration
import System.Directory
import Data.Maybe (isJust)

main :: IO ()
main = hspec $ do
  {- End-to-end tests with no oracle checking -}
  describe "example" $ do
    it "runs ACA without checking against an oracle" $ do
      runTrueTest "example"
  {- End-to-end tests -}
  describe "basic" $ do
    it "runs a basic end-to-end analysis" $
      runTest "basic"
  describe "moveToTop" $ do
    it "runs an analysis that moves to Top" $
      runTest "moveToTop"
  describe "triviallyFalse" $ do
    it "returns a CSC that implicates all inputs" $
      runTest "triviallyFalse"
{-
  {- Tests where an assertion is expected to be thrown -}
  describe "earlyStopping" $ do
    it "runs a single analyzer and stops early" $ do
      testEarlyStopping 
-}
type TestName = String

trueTestCase :: TestName -> FilePath -> Configuration
trueTestCase "example" file =
  Configuration
  { fileParam=file
  , debugParam="full"
  , timeoutParam=60
  , portfolioParam="uAutomizer"
  , generalizeTimeoutParam=60
  , blockValidPathsParam=False
  , exitStrategyParam="eager"
  , genExitStratParam="eager"
  , prefixParam="."
  , targetFunctionParam="__VERIFIER_error"
  , partitionBoundParam=4
  , mergeLengthParam=4
  , genStratParam="pessimisticEq"
  , cppParam=""
  , initTimeoutParam=900
  , excludeParam=""
  , dseParam="civl"
  , makeCudParam=False
  , chewCudParam=""
  , dockerParam=False
  , minusAcaParam=False
  , propertyParam="reachSafety"
  , knownReachParam=False
  }

runTrueTest :: TestName -> IO ()
runTrueTest name = do
  file <- getTestFile name
  let config = trueTestCase name file
  runAca config
  return ()

getTestFile :: TestName -> IO FilePath
getTestFile n = do
  prefix <- getCurrentDirectory
  let testFile = (prefix ++ "/test/programs/" ++n++".c")
  return testFile

runTest :: TestName -> IO ()
runTest name = do
  maybeTest <- testCase name
  if isJust maybeTest
    then do
      let (Just (config, oracle)) = maybeTest
      testCsc <- runAca config
      let testStr = showSmallCsc testCsc
      testStr `shouldBe` oracle
    else do
      putStrLn $ "Could not find the test named: "++name

testAndOracle :: String -> IO (FilePath, String)
testAndOracle n = do
  prefix <- getCurrentDirectory
  let testFile = (prefix ++ "/test/programs/" ++n++".c")
  cscfile <- readFile $ prefix ++ "/test/oracle_cscs/" ++n++".csc"
  return (testFile, cscfile)

testCase :: String -> IO (Maybe (Configuration, String))
testCase "basic" = do
  (testFile, oracle) <- testAndOracle "basic"
  let config = Configuration {
      fileParam=testFile
    , debugParam="full"
    , timeoutParam=60
    , portfolioParam="uAutomizer"
    , generalizeTimeoutParam=60
    , blockValidPathsParam=False
    , exitStrategyParam="eager"
    , genExitStratParam="eager"
    , prefixParam="."
    , targetFunctionParam="__VERIFIER_error"
    , partitionBoundParam=4
    , mergeLengthParam=4
    , genStratParam="pessimisticEq"
    , cppParam=""
    , initTimeoutParam=900
    , excludeParam=""
    , dseParam="cpa"
    , makeCudParam=False
    , chewCudParam=""
    , dockerParam=False
    , minusAcaParam=False
    , propertyParam="reachSafety"
    , knownReachParam=False
    }
  return $ (Just (config, oracle))
testCase "moveToTop" = do
  (testFile, oracle) <- testAndOracle "moveToTop"
  let config = Configuration {
      fileParam=testFile
    , debugParam="full"
    , timeoutParam=60
    , portfolioParam="cbmc"
    , generalizeTimeoutParam=60
    , blockValidPathsParam=False
    , exitStrategyParam="eager"
    , genExitStratParam="eager"
    , prefixParam="."
    , targetFunctionParam="__VERIFIER_error"
    , partitionBoundParam=4
    , mergeLengthParam=4
    , genStratParam="pessimisticEq"
    , cppParam=""
    , initTimeoutParam=900
    , excludeParam=""
    , dseParam="civl"
    , makeCudParam=False
    , chewCudParam=""
    , dockerParam=False
    , minusAcaParam=False
    , propertyParam="reachSafety"
    , knownReachParam=False
    }
  return $ (Just (config, oracle))
testCase "triviallyFalse" = do
  (testFile, oracle) <- testAndOracle "triviallyFalse"
  let config = Configuration {
      fileParam=testFile
    , debugParam="full"
    , timeoutParam=60
    , portfolioParam="uAutomizer"
    , generalizeTimeoutParam=60
    , blockValidPathsParam=False
    , exitStrategyParam="eager"
    , genExitStratParam="eager"
    , prefixParam="."
    , targetFunctionParam="__VERIFIER_error"
    , partitionBoundParam=4
    , mergeLengthParam=4
    , genStratParam="pessimisticEq"
    , cppParam=""
    , initTimeoutParam=900
    , excludeParam=""
    , dseParam="civl"
    , makeCudParam=False
    , chewCudParam=""
    , dockerParam=False
    , minusAcaParam=False
    , propertyParam="reachSafety"
    , knownReachParam=False
    }
  return $ (Just (config, oracle))
{-
testCase "spuriousAndGeneralize" = do
  (testFile, oracle) <- testAndOracle "spuriousAndGeneralize"
  let config = Configuration
               { fileParam=testFile
               , debugParam="generalize"
               , timeoutParam=30
               , portfolioParam="cpaSeq16"
               , statisticsParam=False
               , cscParam=""
               , earlyExitParam=False
               , generalizeTimeoutParam=30
               , blockValidPathsParam=True
               , exitStrategyParam="patient"
               , modularParam="main"
               , sequentializeParam=True
               , slimParam=False
               , prefixParam="."
               }
  return $ (Just (config, oracle))
testCase _ = return Nothing -- handle unknown name in caller

testEarlyStopping :: IO ()
testEarlyStopping = do
  testFile <- getTestFile "basic"
  let config = Configuration
             { fileParam=testFile
             , debugParam="analyzers"
             , timeoutParam=60
             , portfolioParam="cpaSeq"
             , statisticsParam=False
             , cscParam=""
             , earlyExitParam=True
             , generalizeTimeoutParam=60
             , blockValidPathsParam=False
             , exitStrategyParam="eager"
             , modularParam="main"
             , sequentializeParam=False
             , slimParam=False
             , prefixParam="."
             }
  (runAca config) `shouldThrow` anyException
-}