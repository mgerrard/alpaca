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

enrichCscWith :: [Subspace] -> AcaComputation ()
enrichCscWith ss = do
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
