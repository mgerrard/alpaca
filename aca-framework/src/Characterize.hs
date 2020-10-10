module Characterize where

import AcaComputation
import CscTypes
import Solver
import Data.Maybe (catMaybes)
import Data.List (nubBy, sortBy)
import Control.Monad.State
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Foldable (foldrM)
import BinarySearch
import Data.Time
import System.Posix.Process
import System.Exit
import Language.C

characterize :: EvidenceCollection -> AcaComputation Csc
characterize ev = do
  when (triviallyFalse ev) (setTerminationFlag)
  let exactSubspace = characterizeLegitimate ev
  csc <- getCsc

  iterNum <- getIterations
  k <- getPartitionBound

  if (iterNum > 6) -- after 6 iters, we move to Top
    then do
      widen csc Top
    else if (length $ disjointPartitions csc) >= k
      then do
        _ <- generalize csc
        return ()
      else do
        enrichCscWith exactSubspace
        st <- get
        when (makeCud st) (writeCudFilesAndExit exactSubspace)
    
  let validPrefixes = characterizeSpurious ev
  enrichValidSpace validPrefixes
  csc' <- getCsc;
  return csc'

generalize :: Csc -> AcaComputation Csc
generalize csc = do
  incrementGeneralizeCount; startGeneralizeTime
  strategy <- getGenStrategy

  start <- io $ getCurrentTime
  generalization <- singletonSearch strategy
  widen csc generalization
  end <- io $ getCurrentTime
  let genTime = formatFloatN ((realToFrac $ diffUTCTime end start)::Float) 4
  updateLog $ "generalization took "++genTime++"s\n"
  
  endGeneralizeTime
  getCsc

widenCscWithConjunction :: Csc -> Conjunction -> AcaComputation ()
widenCscWithConjunction _ [] = do
  moveToTop
  setTerminationFlag
widenCscWithConjunction csc c = do
  startWideningTime
  csc' <- widenWorker csc c
  endWideningTime
  {- update log -}
  updateLog $ "widening CSC with this conjunction:\n "++(showMinimalConjunction c)++"\n"
  st <- get; put $ st { stateCsc = csc' }

enrichValidSpace :: [Subspace] -> AcaComputation ()
enrichValidSpace subs = do
  csc@(Csc p _ _ ss f) <- getCsc
  let validConjunctions = map conjunction subs
      ss' = ss ++ validConjunctions
      subspaceMaps = map inputIdCounts subs
      subspaceMap = foldl (Map.unionWith max) Map.empty subspaceMaps
      inputMap' = Map.unionWith max (inputCountMap csc) subspaceMap
      subspaceTypeMaps = map inputIdTypes subs
      subspaceTypeMap = foldl (Map.union) Map.empty subspaceTypeMaps
      typeMap' = Map.union (inputTypeMap csc) subspaceTypeMap
      csc' = (Csc p inputMap' typeMap' ss' f)
  
  st <- get
  put $ st { stateCsc = csc' }

characterizeLegitimate :: EvidenceCollection -> [Subspace]
characterizeLegitimate e = catMaybes $ map maybeCharacterization e

characterizeSpurious :: EvidenceCollection -> [Subspace]
characterizeSpurious e = catMaybes $ map maybeSpuriousCharacterization e

updateCscToTriviallyFalse :: AcaComputation ()
updateCscToTriviallyFalse = do
  let ub = makeTrueUpper
      lb = makeTrueLower
      p = CscPartition ub lb []
      csc = Csc [p] Map.empty Map.empty [] Terminate
  st <- get
  put $ st { stateCsc = csc }

enrichCscWith :: [Subspace] -> AcaComputation ()
enrichCscWith ss = do
  if null $ conjunction $ head $ ss
    then updateCscToTriviallyFalse
    else do
      csc@(Csc partitions _ _ s f) <- getCsc
      let subs = reverse $ sortBy (\a b -> (length $ conjunction a) `compare` (length $ conjunction b)) ss
          cs = minimizeConjunctions $ map conjunction subs
          as = map sAssumptions subs
          exactPartitions = map (\(c,a) -> (CscPartition (UpperBound c []) [c] [a])) (zip cs as)
          partitions' = partitions ++ exactPartitions
          subspaceMaps = map inputIdCounts subs
          subspaceMap = foldl (Map.unionWith max) Map.empty subspaceMaps
          inputMap' = Map.unionWith max (inputCountMap csc) subspaceMap
          subspaceTypeMaps = map inputIdTypes subs
          subspaceTypeMap = foldl (Map.union) Map.empty subspaceTypeMaps
          typeMap' = Map.union (inputTypeMap csc) subspaceTypeMap
          csc' = (Csc partitions' inputMap' typeMap' s f)
      st <- get
    
      put $ st { stateCsc = csc' }

minimizeConjunctions :: [Conjunction] -> [Conjunction]
minimizeConjunctions = nubBy (\x y -> x == y)

widen :: Csc -> GeneralizeResult -> AcaComputation ()
widen _ (SafetySpace csc') = do
  updateCsc csc'
  setTerminationFlag
widen _ Top = do
  moveToTop; setTerminationFlag
widen _ (NewSpace csc' ss) = do
  updateCsc csc'
  enrichCscWith [ss]

logFilePrefix :: Program -> FilePath -> FilePath
logFilePrefix (Program _ _ n _ _) logPre = logPre ++ "/logs_alpaca/" ++ n

writeCudFilesAndExit :: [Subspace] -> AcaComputation ()
writeCudFilesAndExit ss = do
  if (null $ conjunction $ head ss)
    then do
      io $ putStrLn "injected error is always hit; not writing .cud files"
      io $ exitImmediately ExitSuccess
    else do
      let subspace = head ss --we assume only one subspace for now
          c = conjunction subspace
          offCs = deriveOffBranches c
      writeFilesAndExit ([c]++offCs)

deriveOffBranches :: Conjunction -> [Conjunction]
deriveOffBranches cs =
  let conjIdPairs = zip cs [0..]
  in map (offBranchPrefix cs) conjIdPairs

offBranchPrefix :: Conjunction -> (Conjunct,Int) -> Conjunction
offBranchPrefix cj (c,i) =
  let negC = negateConjunct c
      pre = take i cj
  in pre++[negC]

negateConjunct :: Conjunct -> Conjunct
negateConjunct (Conjunct expr) =
  let negExpr = CUnary CNegOp expr undefNode
  in Conjunct negExpr

writeFilesAndExit :: [Conjunction] -> AcaComputation ()
writeFilesAndExit cs = do
  csc <- getCsc
  let countMapStr = show $ inputCountMap csc
      typeMapStr = show $ inputTypeMap csc
      lastLines = "\n"++countMapStr++"\n"++typeMapStr

  p <- getProgram; st <- get
  let base = logPrefix st
      progN = pName p
      prefix = (logFilePrefix p base)++"/"++progN
      aStrs = map showConjunction cs
      aStrsIdxs = zip aStrs [1..]
      
  mapM_ (writeCudFiles prefix lastLines) aStrsIdxs
  let exitMsg = (show $ length cs)++" cud files have been produced; our work is done here"
  io $ putStrLn exitMsg
  io $ exitImmediately ExitSuccess

writeCudFiles :: String -> String -> (String, Int) -> AcaComputation ()
writeCudFiles prefix lastLines (assumptionStr, idx) = do
  let i = show idx
      partitionName = prefix++".p"++i++".cud"
      partitionStr = assumptionStr++lastLines
  io $ writeFile partitionName partitionStr
