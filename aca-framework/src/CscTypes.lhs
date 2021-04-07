\begin{code}
module CscTypes where
import qualified Data.Map.Strict as Map
import Branch
import Portfolio
import Language.C

data Csc = Csc {
  --  upperBounds must be logically disjoint
  disjointPartitions :: [CscPartition],
  inputCountMap :: CountMap,
  inputTypeMap :: TypeMap,
  spuriousSpace :: [Conjunction],
  searchFlag :: SearchFlag
  } deriving (Show, Eq)

data SearchFlag = Terminate | Search deriving (Show, Read, Eq)

type CountMap = Map.Map Int Int
type TypeMap = Map.Map Int TypeString

data CscPartition = CscPartition {
  upperBound :: UpperBound,
  lowerBound :: LowerBound,
  assumptions :: [Conjunction]
  } deriving (Show, Eq)

data UpperBound = UpperBound {
  upper :: Conjunction,
  upperNegations :: [Conjunction]
  } deriving (Show, Eq)
data LowerBound = LowerBound {
  lower :: [Conjunction],
  nSliced :: Int 
  } deriving (Show, Eq)

equivalentPartitionSize :: Csc -> Int
equivalentPartitionSize (Csc parts _ _ _ _) =
  (length . filter isEquivalentPartition) parts

isEquivalentPartition :: CscPartition -> Bool
isEquivalentPartition (CscPartition _ (LowerBound [] _) _) = False
isEquivalentPartition (CscPartition up lo _) = (head $ lower lo) == (upper up)

gapBetweenBounds :: CscPartition -> Bool
gapBetweenBounds = not . isEquivalentPartition

type Conjunction = [Conjunct]
data Conjunct = Conjunct CExpr
instance Show Conjunct where
  show (Conjunct c) = show $ pretty c
instance Eq Conjunct where
  (Conjunct e1) == (Conjunct e2) =
    (show (pretty e1)) == (show (pretty e2))
instance Ord Conjunct where
  (Conjunct e1) `compare` (Conjunct e2) =
    (show (pretty e1)) `compare` (show (pretty e2))

data RawConjunction = RawConjunction [RawConjunct] Int deriving (Show, Eq)
data RawConjunct = RawConjunct String deriving (Show, Eq)

type VariableName = String
type CeeType = String

type Domain = [InputVariable]
type InputVariable = (VariableName, CeeType)
type TypeString = String

data ExplorationResult =
  UnreachableEvidence |
  ReachableEvidence EvidenceCollection |
  NoEvidence

data PreSubspace = PreSubspace {
    pcToBlock :: RawConjunction,
    pAssumptions :: RawConjunction,
    pCountMap :: CountMap,
    pTypeMap :: TypeMap
  }
  deriving (Show, Eq)

data Subspace = Subspace {
  conjunction :: Conjunction,
  sAssumptions :: Conjunction,
  inputIdCounts :: CountMap,
  inputIdTypes :: TypeMap,
  nSlice :: Int
  } deriving (Show)

instance Eq Subspace where
  (Subspace c1 _ _ _ _) == (Subspace c2 _ _ _ _) = c1 == c2

data InputMapping = InputMapping
  { symbolicInputVar :: String,
    symbolicInputType :: String,
    acaInputId :: Int,
    inputArrayIndex :: Int,
    iterationCount :: Int
  } deriving (Show, Eq)

data AnalysisWitness = AnalysisWitness {
  analyzer :: Analyzer,
  programPath :: FilePath,
  witnessPath :: FilePath,
  isSafe :: Bool,
  branches :: [Branch],
  analysisTime :: Float,
  parsingTime :: Float}
  deriving (Show, Eq)

{- TODO: pass this in as a command line arg -}
data DseTool = CivlSymExec | CpaSymExec deriving (Show, Eq)

type EvidenceCollection = [PieceOfEvidence]

emptyWitness :: AnalysisWitness
emptyWitness = AnalysisWitness emptyAnalyzer "" "" False [] 0 0

civlWitness :: AnalysisWitness
civlWitness = AnalysisWitness civlAnalyzer "" "" False [] 0 0

maybeCharacterization :: PieceOfEvidence -> Maybe Subspace
maybeCharacterization (LegitimateEvidence _ c _) = Just c
maybeCharacterization _ = Nothing

maybeSpuriousCharacterization :: PieceOfEvidence -> Maybe Subspace
maybeSpuriousCharacterization (SpuriousEvidence _ c _) = Just c
maybeSpuriousCharacterization _ = Nothing

data PieceOfEvidence =
  LegitimateEvidence {
  analysisWitness :: AnalysisWitness,
  characterization :: Subspace,
  legitDirectedTime :: Float
  }
  | SpuriousEvidence {
  validWitness :: AnalysisWitness,
  spuriousCharacterization :: Subspace,
  spuriousDirectedTime :: Float
  }
  | TrivialEvidence
  | EmptyEvidence {
  failedWitness :: AnalysisWitness
  } deriving (Show, Eq)

data GeneralizeResult =
    SafetySpace Csc
  | NewSpace Csc Subspace
  | Top
  deriving (Show, Eq)

--data SafetySpace =
--  SafetySpace Csc
--  | Top
--  deriving (Show, Eq)

type Estimate = Rational
type Variance = Rational

data CounterResult =
  ExactResult Estimate
  | StatisticalResult Estimate Variance
  deriving (Show, Eq)

data ExitStrategy = Eager | Patient deriving (Show, Eq)
data GenStrategy =
    PessimisticEq
  | PessimisticDisEq
  | OptimisticEq
  | OptimisticDisEq
  deriving (Show, Eq)
data Parallelism = InParallel | InSequence deriving (Show, Eq)

data RunConfiguration = RunConfiguration {
  runParallelism :: Parallelism,
  runDebug :: DebugMode,
  maybeTag :: Maybe Int,
  runExitStrategy :: ExitStrategy,
  runBlockValid :: Bool,
  runLogPrefix :: String,
  runExitFile :: FilePath,
  runDseTool :: DseTool,
  runDocker :: Bool,
  runPropFile :: Property,
  runIsMinAca :: Bool
  }

data DebugMode =
  Full |
  Slice |
  Analyzers |
  Direct |
  Generalize |
  Quiet deriving (Show, Eq)
\end{code}
