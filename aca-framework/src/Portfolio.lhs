The following 19 analysis tools were chosen to be in
ALPACA because
they were contestants in the 2022 Software Verification
Competition (SV-COMP) in one (or more) of the three
categories: ReachSafety, SoftwareSystems, or
FalsificationOverall. (DIVINE was dropped because its
performance was terrible. Crux was dropped because its
performance contained too many false negatives, which
we do not have the ability to refute at this point, i.e.,
ALPACA short-circuits upon finding a TRUE result. Infer
is dropped because their poor performance is off-the-charts:
-50,000 in ReachSafety, so bad that they do not appear
on the generated gnuplots.)

The tools are:

- 2LS
- CBMC
- CPA-BAM-BnB
- CPA-BAM-SMG
- CPAchecker-2-1
- ESBMC-kind
- Goblint
- Graves-CPA
- LART
- PeSCo
- Pinaka
- SMACK
- Symbiotic
- Theta
- UAutomizer
- UKojak
- UTaipan
- VeriAbs
- VeriFuzz

We additionally include CIVL and SeaHorn as
verifiers; these tools participated in previous
SV-COMP years.

\begin{code}
module Portfolio where

import LocalPaths
import Data.List.Split (splitOn)
import Data.List (find, nub, (\\))
import Data.Maybe (catMaybes)

type Portfolio = [Analyzer]
data Property = ReachSafety | MemSafety | OverflowSafety deriving (Show, Read, Eq)

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
  | CPA_BAM_SMG
  | CPA_Seq
  | CPA_Validator
  | DepthK
  | ESBMC
  | Goblint
  | Graves
  | InterpChecker
  | LART
  | Pesco
  | Pinaka
  | SMACK
  | Symbiotic
  | Seahorn
  | Theta
  | UAutomizer
  | UKojak
  | UTaipan
  | VeriAbs
  | VeriFuzz deriving (Eq, Show)

data WitnessType = BranchDirectives | ConcreteInputs | NoWitness

instance Eq Analyzer where
  x == y = analysisTool x == analysisTool y
  
instance Show Analyzer where
  show (Analyzer name _ _ _ _ _ _ _ _) = show name

type Attribute = String
type Value = String
type Option = (Attribute, Maybe Value)

portfolio :: String -> Property ->String -> Int -> Int -> Int -> IO Portfolio
portfolio pFilter prop exclusions timeout gTimeout iTimeout = do
  full <- fullPortfolio timeout gTimeout iTimeout
  let subset = nub $ portfolioSubset pFilter exclusions full prop
  return subset

portfolioSubset :: String -> String -> Portfolio -> Property -> Portfolio
portfolioSubset "all" "" p prop = filter wheatFromChaff (sheepFromGoat prop p)
portfolioSubset "all" exclusions p prop =
  let wheat = filter wheatFromChaff (sheepFromGoat prop p)
      stringExclusions = splitOn "," exclusions
      mExclusions = map (correspondingTool p) stringExclusions
      pExclusions = catMaybes mExclusions
  in wheat \\ pExclusions
portfolioSubset "allDock" _ p _ = filter dockerSubset p
portfolioSubset pFilter exclusions p _ =
  let stringSelections = splitOn "," pFilter
      stringExclusions = splitOn "," exclusions
      stringSelections' = stringSelections \\ stringExclusions
      mSelections = map (correspondingTool p) stringSelections'
  in catMaybes mSelections

correspondingTool :: Portfolio -> String -> Maybe Analyzer
correspondingTool p "veriFuzz" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==VeriFuzz) p
correspondingTool p "theta" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Theta) p
correspondingTool p "smack" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==SMACK) p
correspondingTool p "lart" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==LART) p
correspondingTool p "graves" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Graves) p
correspondingTool p "goblint" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Goblint) p
correspondingTool p "cpaBamBnB" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CPA_BAM_BnB) p
correspondingTool p "cpaBamSmg" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CPA_BAM_SMG) p
correspondingTool p "cpaSeq" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CPA_Seq) p
correspondingTool p "uAutomizer" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==UAutomizer) p
correspondingTool p "uKojak" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==UKojak) p
correspondingTool p "uTaipan" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==UTaipan) p
correspondingTool p "veriAbs" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==VeriAbs) p
correspondingTool p "cbmc" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CBMC) p
correspondingTool p "civl" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==CIVL) p
correspondingTool p "twoLs" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==TwoLS) p
correspondingTool p "esbmc" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==ESBMC) p
correspondingTool p "symbiotic" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Symbiotic) p
correspondingTool p "seahorn" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Seahorn) p
correspondingTool p "pesco" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Pesco) p
correspondingTool p "pinaka" = find (\(Analyzer a _ _ _ _ _ _ _ _)->a==Pinaka) p
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

{- The following are the tools that report a reasonable amount of evidence
   and can be run in Docker -}
dockerSubset :: Analyzer -> Bool
dockerSubset (Analyzer CPA_Seq _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer CPA_BAM_BnB _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer UAutomizer _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer UTaipan _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer VeriAbs _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer ESBMC _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer Symbiotic _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer CBMC _ _ _ _ _ _ _ _) = True
dockerSubset (Analyzer Pesco _ _ _ _ _ _ _ _) = True
--dockerSubset (Analyzer Seahorn _ _ _ _ _ _ _ _) = True -- no SeaHorn for baseline
dockerSubset _ = False

{- Used to determine which tools do MemSafety and OverflowSafety -}
sheepFromGoatHelper :: Property -> Analyzer -> Bool
sheepFromGoatHelper _ (Analyzer CPA_Seq _ _ _ _ _ _ _ _) = True
sheepFromGoatHelper _ (Analyzer CPA_BAM_BnB _ _ _ _ _ _ _ _) = False
sheepFromGoatHelper _ (Analyzer UAutomizer _ _ _ _ _ _ _ _) = True
sheepFromGoatHelper _ (Analyzer UTaipan _ _ _ _ _ _ _ _) = True
sheepFromGoatHelper _ (Analyzer VeriAbs _ _ _ _ _ _ _ _) = False
sheepFromGoatHelper _ (Analyzer ESBMC _ _ _ _ _ _ _ _) = True
sheepFromGoatHelper _ (Analyzer Symbiotic _ _ _ _ _ _ _ _) = True
sheepFromGoatHelper _ (Analyzer CBMC _ _ _ _ _ _ _ _) = True
sheepFromGoatHelper _ (Analyzer Pesco _ _ _ _ _ _ _ _) = False
sheepFromGoatHelper _ _ = False

sheepFromGoat :: Property -> Portfolio -> Portfolio
sheepFromGoat ReachSafety l = l
sheepFromGoat MemSafety l = filter (sheepFromGoatHelper MemSafety) l
sheepFromGoat OverflowSafety l = filter (sheepFromGoatHelper OverflowSafety) l

fullPortfolio :: Int -> Int -> Int -> IO Portfolio
fullPortfolio timeout gTimeout iTimeout = do
  portfolioDir <- getPortfolioDir
  let
    cpabamsmg = Analyzer {
      analysisTool = CPA_BAM_SMG,
      analysisName = "cpabamsmg",
      analysisDir = portfolioDir ++ "CPA_BAM_SMG",
      analysisOptions = [
        ("-svcomp21-bam-smg", Nothing),
        ("-heap", Just "10000M"),
        ("-disable-java-assertions", Nothing),
        ("-setprop", Just "cfa.allowBranchSwapping=false"),
        ("-setprop", Just "cpa.arg.witness.exportSourcecode=true"),
        ("-timelimit", Just "900 s")],
      -- think this is safe
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    cpabambnb = Analyzer {
      analysisTool = CPA_BAM_BnB,
      analysisName = "cpabambnb",
      analysisDir = portfolioDir ++ "CPA_BAM_BnB",
      analysisOptions = [
        ("-svcomp21-bam-bnb", Nothing),
        ("-heap", Just "10000M"),
        ("-disable-java-assertions", Nothing),
        ("-setprop", Just "cfa.allowBranchSwapping=false"),
        ("-setprop", Just "cpa.arg.witness.exportSourcecode=true"),
        ("-timelimit", Just "900 s")
      ],
      -- think this is safe
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    goblint = Analyzer {
      analysisTool = Goblint,
      analysisName = "goblint",
      analysisDir = portfolioDir ++ "Goblint",
      analysisOptions = [("--conf", Just "conf/svcomp22.json")],
      -- assume this is just linter, i.e., underapprox
      safeOverapproximation = False,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    graves = Analyzer {
      analysisTool = Graves,
      analysisName = "graves",
      analysisDir = portfolioDir ++ "Graves",
      analysisOptions = [
        ("-svcomp22-graves", Nothing),
        ("-heap", Just "10000M"),
        ("-benchmark", Nothing),
        ("-setprop", Just "cfa.allowBranchSwapping=false"),
        ("-setprop", Just "cpa.arg.witness.exportSourcecode=true"),
        ("-timelimit", Just "900 s")],
      -- uses safe CPA analyses (check with Will)
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    lart = Analyzer {
      analysisTool = LART,
      analysisName = "lart",
      analysisDir = portfolioDir ++ "LART",
      analysisOptions = [],
      -- unsure, but looks like a safe analysis?
      safeOverapproximation = False,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    smack = Analyzer {
      analysisTool = SMACK,
      analysisName = "smack",
      analysisDir = portfolioDir ++ "SMACK",
      analysisOptions = [("-w", Just "error-witness.graphml")],
      -- verifies up to a bound
      safeOverapproximation = False,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    theta = Analyzer {
      analysisTool = Theta,
      analysisName = "theta",
      analysisDir = portfolioDir ++ "theta",
      analysisOptions = [
        ("--witness-only", Nothing),
	("--portfolio", Just "COMPLEX"),
	("--loglevel", Just "RESULT")
      ],
      -- model checker based on abstr. refinement
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    veriFuzz = Analyzer {
      analysisTool = VeriFuzz,
      analysisName = "verifuzz",
      analysisDir = portfolioDir ++ "VeriFuzz",
      analysisOptions = [],
      -- evolutionary fuzz testing w/ static analysis (safe)
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    cpaSeq = Analyzer {
      analysisTool = CPA_Seq,
      analysisName = "cpachecker",
      analysisDir = portfolioDir ++ "CPA_Seq",
      analysisOptions = [
--        ("-svcomp22", Nothing),
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
      analysisOptions = [("--full-output", Nothing)],
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
        ("--sv-comp", Nothing)],
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
          ("-svcomp21-pesco", Nothing),
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
    twoLs = Analyzer {
      analysisTool = TwoLS,
      analysisName = "two_ls",
      analysisDir = portfolioDir ++ "TwoLS",
      analysisOptions = [
        ("--graphml-witness", Just "witness.graphml")],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = ConcreteInputs,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    cbmc = Analyzer {
      analysisTool = CBMC,
      analysisName = "cbmc",
      analysisDir = portfolioDir ++ "CBMC",
      analysisOptions = [
        ("--graphml-witness", Just "witness.graphml")],
      safeOverapproximation = False,
      analysisTimeout = timeout,
      witnessType = ConcreteInputs,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    uTaipan = Analyzer {
      analysisTool = UTaipan,
      analysisName = "ultimatetaipan",
      analysisDir = portfolioDir ++ "UTaipan",
      analysisOptions = [("--full-output", Nothing)],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    uKojak = Analyzer {
      analysisTool = UKojak,
      analysisName = "ultimatekojak",
      analysisDir = portfolioDir ++ "UKojak",
      analysisOptions = [("--full-output", Nothing)],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = BranchDirectives,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    pinaka = Analyzer {
      analysisTool = Pinaka,
      analysisName = "pinaka",
      analysisDir = portfolioDir ++ "Pinaka",
      analysisOptions = [("--graphml-witness", Just "witness.graphml")],
      safeOverapproximation = True, -- only terminates when all paths are examined
      analysisTimeout = timeout,
      witnessType = ConcreteInputs,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
    seahorn = Analyzer {
      analysisTool = Seahorn,
      analysisName = "seahorn",
      analysisDir = portfolioDir ++ "Seahorn",
      analysisOptions = [("--cex=witness.graphml", Nothing)],
      safeOverapproximation = True,
      analysisTimeout = timeout,
      witnessType = ConcreteInputs,
      generalizeTimeout = gTimeout,
      initTimeout = iTimeout
      }
  return [cpaSeq,uAutomizer,esbmc,pesco,symbiotic,veriAbs,twoLs,cbmc,uTaipan,uKojak,pinaka,seahorn,cpabamsmg,cpabambnb,goblint,graves,lart,smack,theta,veriFuzz]
\end{code}
