module Portfolio where

import LocalPaths
import Data.List.Split (splitOn)
import Data.List (find, nub, (\\))
import Data.Maybe (catMaybes)

type Portfolio = [Analyzer]

getAnalyzerDir :: IO String
getAnalyzerDir = do
  a <- analyzerDir
  return a

getPortfolioDir :: IO String
getPortfolioDir = do
  a <- analyzerDir
  return $ a ++ "/svcomp_archives/"

emptyAnalyzer :: Analyzer
emptyAnalyzer = Analyzer CBMC "" "" [] False 0 NoWitness 0 0

civlAnalyzer :: Analyzer
civlAnalyzer = Analyzer CIVL "" "" [] False 0 NoWitness 0 0

data Analyzer = Analyzer {
  analysisTool :: AnalysisTool,
  analysisName :: String,
  analysisDir :: FilePath,
  analysisOptions :: [Option],
  safeOverapproximation :: Bool,
  analysisTimeout :: Int,
  witnessType :: WitnessType,
  generalizeTimeout :: Int,
  initTimeout :: Int
  }

data AnalysisTool =
    TwoLS
  | CBMC
  | CIVL
  | CPA_BAM_BnB
  | CPA_BAM_Slicing
  | CPA_Seq
  | CPA_Seq_16
  | CPA_Validator
  | DepthK
  | ESBMC
  | IKOS
  | InterpChecker
  | Pesco
  | Symbiotic
  | Seahorn
  | Smack
  | UAutomizer
  | UKojak
  | UTaipan
  | VeriAbs deriving (Eq, Show)

data WitnessType = BranchDirectives | ConcreteInputs | NoWitness

instance Eq Analyzer where
  x == y = analysisTool x == analysisTool y
  
instance Show Analyzer where
  show (Analyzer name _ _ _ _ _ _ _ _) = show name

type Attribute = String
type Value = String
type Option = (Attribute, Maybe Value)

portfolio :: String -> String -> Int -> Int -> Int -> IO Portfolio
portfolio pFilter exclusions timeout gTimeout iTimeout = do
  full <- fullPortfolio timeout gTimeout iTimeout
  let subset = nub $ portfolioSubset pFilter exclusions full
  return subset

portfolioSubset :: String -> String -> Portfolio -> Portfolio
portfolioSubset "all" "" p = filter wheatFromChaff p
portfolioSubset "all" exclusions p =
  let wheat = filter wheatFromChaff p
      stringExclusions = splitOn "," exclusions
      mExclusions = map (correspondingTool p) stringExclusions
      pExclusions = catMaybes mExclusions
  in wheat \\ pExclusions
portfolioSubset pFilter exclusions p =
  let stringSelections = splitOn "," pFilter
      stringExclusions = splitOn "," exclusions
      stringSelections' = stringSelections \\ stringExclusions
      mSelections = map (correspondingTool p) stringSelections'
  in catMaybes mSelections

correspondingTool :: Portfolio -> String -> Maybe Analyzer
correspondingTool p "cpaSeq" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CPA_Seq) p
correspondingTool p "cpaBamBnb" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CPA_BAM_BnB) p
correspondingTool p "cpaBamSlicing" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CPA_BAM_Slicing) p
correspondingTool p "interpChecker" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==InterpChecker) p
correspondingTool p "uAutomizer" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==UAutomizer) p
correspondingTool p "uKojak" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==UKojak) p
correspondingTool p "uTaipan" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==UTaipan) p
correspondingTool p "veriAbs" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==VeriAbs) p
correspondingTool p "cbmc" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CBMC) p
correspondingTool p "twoLs" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==TwoLS) p
correspondingTool p "depthK" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==DepthK) p
correspondingTool p "esbmc" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==ESBMC) p
correspondingTool p "symbiotic" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Symbiotic) p
correspondingTool p "pesco" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Pesco) p
correspondingTool p "cpaSeq16" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CPA_Seq_16) p
correspondingTool p "seahorn" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Seahorn) p
correspondingTool p "smack" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Smack) p
correspondingTool p "civl" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CIVL) p
correspondingTool _ t = error $ "Sorry, but I don't recognize the tool: "++t

{- The following are the tools that report a reasonable amount of evidence -}
wheatFromChaff :: Analyzer -> Bool
wheatFromChaff (Analyzer CPA_Seq _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer CPA_BAM_BnB _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer UAutomizer _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer UTaipan _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer VeriAbs _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer ESBMC _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer Symbiotic _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer CBMC _ _ _ _ _ _ _ _) = True
wheatFromChaff (Analyzer Pesco _ _ _ _ _ _ _ _) = True
wheatFromChaff _ = False

fullPortfolio :: Int -> Int -> Int -> IO Portfolio
fullPortfolio timeout gTimeout iTimeout = do
  portfolioDir <- getPortfolioDir
  let
    cpaSeq = Analyzer {
      analysisTool = CPA_Seq,
      analysisName = "cpachecker",
      analysisDir = portfolioDir ++ "CPA_Seq",
      analysisOptions = [
        ("-svcomp20", Nothing),
        ("-heap", Just "10000M"),
        ("-benchmark", Nothing),
        ("-setprop", Just "cfa.allowBranchSwapping=false"),
        ("-setprop", Just "cpa.arg.witness.exportSourcecode=true"),
        ("-timelimit", Just "900 s")],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    uAutomizer = Analyzer {
      analysisTool = UAutomizer,
      analysisName = "ultimateautomizer",
      analysisDir = portfolioDir ++ "UAutomizer",
      analysisOptions = [
        ("--full-output", Nothing),
        ("--architecture", Just "32bit")],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    symbiotic = Analyzer {
      analysisTool = Symbiotic,
      analysisName = "symbiotic",
      analysisDir = portfolioDir ++ "Symbiotic",
      analysisOptions = [
        ("--witness", Just "witness.graphml"),
	("--sv-comp", Nothing),
        ("--32", Nothing)],
      safeOverapproximation = False,
      analysisTimeout = timeout,
      witnessType = ConcreteInputs,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }      
    veriAbs = Analyzer {
      analysisTool = VeriAbs,
      analysisName = "veriabs",
      analysisDir = portfolioDir ++ "VeriAbs",
      analysisOptions = [], -- no options
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    esbmc = Analyzer {
      analysisTool = ESBMC,
      analysisName = "esbmc",
      analysisDir = portfolioDir ++ "ESBMC",
      analysisOptions = [
          ("-s", Just "kinduction")],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = ConcreteInputs,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    pesco = Analyzer {
      analysisTool = Pesco,
      analysisName = "pesco",
      analysisDir = portfolioDir ++ "Pesco",
      analysisOptions = [
          ("-svcomp20-pesco", Nothing),
          ("-heap", Just "10000M"),
          ("-stack", Just "2048k"),
          ("-benchmark", Nothing),
          ("-timelimit", Just "900 s")],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
  return [cpaSeq,uAutomizer,esbmc,pesco,symbiotic,veriAbs]
