module Solver where

import Data.SBV
import SolverLib
import CscTypes
import AcaComputation
import Data.List (find, nub)
import Data.Time

enforceDisjointness :: Csc -> AcaComputation Csc
enforceDisjointness csc = do
  start <- io $ getCurrentTime
  partitions <- mapM updateOverlaps (zip [0..] (disjointPartitions csc))
  end <- io $ getCurrentTime
  let t = formatFloatN ((realToFrac $ diffUTCTime end start)::Float) 4
  updateLog $ "enforcing disjointness took "++t++"s\n"
  let csc' = csc { disjointPartitions = partitions }
  return csc'

updateOverlaps :: (Int, CscPartition) -> AcaComputation CscPartition
updateOverlaps (idx, (CscPartition (UpperBound u _) lb as)) = do
  csc <- getCsc
  let precedingPs = take idx (disjointPartitions csc)
  negations <- findExcludedFormulae u precedingPs
  let ub = UpperBound u negations
  return $ CscPartition ub lb as

findExcludedFormulae :: Conjunction -> [CscPartition] -> AcaComputation [Conjunction]
findExcludedFormulae u ps = do
  let uppers = map (upper . upperBound) ps

  {- Intersections -}
  inResults <- mapM (intersects u) (zip [1..] uppers)
  let firstSolverCalls = length uppers
      intersections = map fst $ filter isSatRes inResults
      unknowns1 = map fst $ filter isUnknownRes inResults

  {- Implications -}
  imResults <- mapM (implies u) (zip [1..] intersections)
  let secondSolverCalls = length intersections
      numSolverCalls = firstSolverCalls + secondSolverCalls
  increaseSolverCallCount numSolverCalls
  let partials = map fst $ filter hasCounterExample imResults
      unknowns2 = map fst $ filter proofUnknown imResults

      excludedFormulae = partials ++ unknowns1 ++ unknowns2

  return excludedFormulae
      
widenWorker :: Csc -> Conjunction -> AcaComputation Csc
widenWorker csc blockingClause = do

  let uppers = upperBounds csc

  {- Intersections -}
  inResults <- mapM (intersects blockingClause) (zip [1..] uppers)
  let firstSolverCalls = length uppers
      intersections = map fst $ filter isSatRes inResults
      unknowns1 = map fst $ filter isUnknownRes inResults

  {- Implications -}
  imResults <- mapM (implies blockingClause) (zip [1..] intersections)
  let secondSolverCalls = length intersections
      numSolverCalls = firstSolverCalls + secondSolverCalls
  increaseSolverCallCount numSolverCalls
  let subsumed = map fst $ filter isProven imResults
      partials = map fst $ filter hasCounterExample imResults
      unknowns2 = map fst $ filter proofUnknown imResults

      excludedFormulae = partials ++ unknowns1 ++ unknowns2
      theBlockingClause = UpperBound blockingClause excludedFormulae
      
      {- Collect the children of the subsumed -}
      subsumptions = concat $ map (findLowerBound csc) subsumed
      assumes = concat $ map (findAssumptions csc) subsumed
      partition = makeNewPartition theBlockingClause subsumptions assumes
      csc' = csc `removeSubsumed` subsumed
      csc'' = csc' `addPartition` partition
  return csc''

makeNewPartition :: UpperBound -> [Conjunction] -> [Conjunction] -> CscPartition
makeNewPartition c [] a = CscPartition c [(upper c)] a
makeNewPartition c subsumptions a = CscPartition c subsumptions a

implies :: Conjunction -> (Int, Conjunction) -> AcaComputation (Conjunction, ThmResult)
implies up (_, c1) = do
  let antecedent = makeConjunctionExpr $ map (\(Conjunct c)->c) c1
      consequent = makeConjunctionExpr $ map (\(Conjunct c)->c) up
      aNames = map extractVarName $ extractCIndexes antecedent
      cNames = map extractVarName $ extractCIndexes consequent
      env = nub (aNames ++ cNames)
  result <- io $ impliesWorker antecedent consequent env
  return (c1, result)

intersects :: Conjunction -> (Int, Conjunction) -> AcaComputation (Conjunction, SatResult)
intersects gen (_, up) = do
  let gen' = makeConjunctionExpr $ map (\(Conjunct c)->c) gen
      up' = makeConjunctionExpr $ map (\(Conjunct c)->c) up
      genAndUp = makeConjunctionExpr [up',gen']
      env = nub $ map extractVarName $ extractCIndexes genAndUp
  result <- io $ isSat genAndUp env
  return (up, result)

checkBoundSatisfiability :: PieceOfEvidence -> AcaComputation ()
checkBoundSatisfiability (LegitimateEvidence (AnalysisWitness _ _ _ False _ _ _) (Subspace cs _ _ _) _) = do
  let c' = makeConjunctionExpr $ map (\(Conjunct c)->c) cs
  let names = nub $ map extractVarName $ extractCIndexes c'
  _ <- io $ isSat c' names
  return ()
checkBoundSatisfiability _ = return ()

findLowerBound :: Csc -> Conjunction -> [Conjunction]
findLowerBound csc c =
  let partitions = disjointPartitions csc
      (Just p) = find (\part -> c == (upperConjunction part)) partitions
  in lowerBound p

findAssumptions :: Csc -> Conjunction -> [Conjunction]
findAssumptions csc c =
  let partitions = disjointPartitions csc
      (Just p) = find (\part -> c == (upperConjunction part)) partitions
  in assumptions p
  
upperConjunction :: CscPartition -> Conjunction
upperConjunction = (upper . upperBound)
  
removeSubsumed :: Csc -> [Conjunction] -> Csc
removeSubsumed csc [] = csc
removeSubsumed csc subsumed =
  let ps = disjointPartitions csc
      ps' = filter (\(CscPartition up _ _) -> not $ elem (upper up) subsumed) ps
  in csc { disjointPartitions = ps' }

addPartition :: Csc -> CscPartition -> Csc
addPartition csc p =
  let ps = (disjointPartitions csc) ++ [p]
  in csc { disjointPartitions = ps }

coversDomain :: Csc -> AcaComputation Bool
coversDomain csc = do
  let cs = upperBounds csc
      upperConjuncts = map (\conj->makeConjunctionExpr $ map (\(Conjunct c)->c) conj) cs
      disj = makeDisjunctionExpr upperConjuncts
      env = nub $ map extractVarName $ extractCIndexes disj
  r <- io $ disjunctionEqualsTrue disj env
  if (impliesTrue r)
    then return True
    else return False

