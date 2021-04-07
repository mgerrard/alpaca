\begin{code}
module BinarySearch where

import Data.List (tails, zip4)
import System.Directory (createDirectoryIfMissing)
import CscTypes
import AcaComputation
import Control.Exception
import Control.Exception.Assert
import Control.Concurrent.Async
import Control.Monad.State
import Transformer
import Language.C
import Data.Maybe (catMaybes, isJust)
import Data.Time
import RunPortfolio
import Portfolio
import Solver
import SolverLib
import Data.SBV hiding (isSafe)
import Data.List (nub, sort, sortBy)

type BlockEvidence = [(PieceOfEvidence, Conjunction, Csc, Float)]

runOnBlocks :: Csc -> SearchContext -> [LatticeElement] -> BlockEvidence -> AcaComputation BlockEvidence
runOnBlocks _ _ [] ev = return ev
runOnBlocks csc ctx elems ev = do
  {- Reasonable limit for the reduced portfolio on doppio's 64-core machines -}
  let blockSize = 9

  _ <- if ((length elems) > blockSize)
         then do
           updateLog $ "running blocking in chunks of "++(show blockSize)++" until new evidence is found\n\n"
           return []
         else return []
    
  rs <- runGenPortfolio ctx (take blockSize elems)

  let rs' = concatQuads $ catMaybes rs
      newEvidence = filter (isNewGenEvidence csc) rs'

  if null newEvidence
    then runOnBlocks csc ctx (drop blockSize elems) ev
    else do
      genStrat <- getGenExitStrategy
      if (genStrat == Eager)
        then do
          let evTup = map (\(e,c,_,t)->(e,c,t)) newEvidence
          let r = makeGenResultLog evTup
          updateLog r
          return newEvidence
        else do {- keep on examining the tuples -}
          let evTup = map (\(e,c,_,t)->(e,c,t)) newEvidence
          let r = makeGenResultLog evTup
          updateLog r
          runOnBlocks csc ctx (drop blockSize elems) (newEvidence++ev)

bestOverapproximators :: Analyzer -> Bool
bestOverapproximators (Analyzer VeriAbs _ _ _ _ _ _ _ _) = True
bestOverapproximators (Analyzer CPA_BAM_BnB _ _ _ _ _ _ _ _) = True
bestOverapproximators (Analyzer UAutomizer _ _ _ _ _ _ _ _) = True
bestOverapproximators (Analyzer CPA_Seq _ _ _ _ _ _ _ _) = True
bestOverapproximators (Analyzer Seahorn _ _ _ _ _ _ _ _) = True
bestOverapproximators _ = False

singletonSearch :: GenStrategy -> AcaComputation GeneralizeResult
singletonSearch strat = do
  st <- get
  let (AcaState prog csc pfolio d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = st
  let smallPfolio = filter bestOverapproximators pfolio
  let debug = ((d == Full) || (d == Generalize))
  if debug then do io $ putStrLn "** Generalizing **" else return ()
  byPred assert "There is some partition describing reachability"
    (not . null) (disjointPartitions csc) $ return ()
  {- update log by describing strategy -}    
  prefix <- getPathPrefix
  io $ createDirectoryIfMissing True prefix
  let c = shortestConjWithEq csc

  {- Set up instrumentation/logging context -}
  let m = 1
  let ctx = SearchContext csc c prog prefix smallPfolio d
  let p' = pathPrefix ctx
  let levelPrefix = p' ++ "/" ++ (show m)
  let ctx' = ctx { pathPrefix=levelPrefix }
  io $ createDirectoryIfMissing True levelPrefix

  levelElements <- getLevelElements strat m ctx' {- singletons -}
  {- update log -}
  io $ putStrLn $ "Let's try blocking singletons from "++(show c)
  updateLog $ "trying to block singletons of:\n "++(showMinimalConjunction c)++"\n\n"
  newEvidence <- runOnBlocks csc ctx' levelElements []
  
  {- We first try the singletons, if that fails, we move to Top -}
  if null newEvidence
    then do
      io $ putStrLn $ "\nCould not block any singletons"
      io $ putStrLn $ " -> Moving to Top"

      {- update log -}
      updateLog "\ncould not block any singletons\n"
      updateLog "-> moving to top\n"
      
      return Top
    else do
      successes <- io $ successfulBlockingClauses newEvidence
      io $ putStrLn $ "We could block these clauses:\n"++(show successes)
      {- update log -}
      updateLog "...picking a random singleton\n\n"

      singletonUpper <- io $ pickSubspace newEvidence (runningLog st)
      return singletonUpper

heuristicSearch :: GenStrategy -> AcaComputation GeneralizeResult
heuristicSearch strat = do
  st <- get
  let (AcaState prog csc pfolio d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = st
  let debug = ((d == Full) || (d == Generalize))
  if debug then do io $ putStrLn "** Generalizing **" else return ()
  byPred assert "There is some partition describing reachability"
    (not . null) (disjointPartitions csc) $ return ()
  {- update log by describing strategy -}    
  prefix <- getPathPrefix
  io $ createDirectoryIfMissing True prefix
  let c = lastAddedConjunction csc

  {- Set up instrumentation/logging context -}
  let m = 1
  let ctx = SearchContext csc c prog prefix pfolio d
  let p' = pathPrefix ctx
  let levelPrefix = p' ++ "/" ++ (show m)
  let ctx' = ctx { pathPrefix=levelPrefix }
  io $ createDirectoryIfMissing True levelPrefix

  levelElements <- getLevelElements strat m ctx' {- singletons -}
  {- update log -}
  io $ putStrLn $ "Let's try blocking singletons from "++(show c)
  updateLog $ "trying to block singletons of:\n "++(showMinimalConjunction c)++"\n\n"
  newEvidence <- runOnBlocks csc ctx' levelElements []
  
  {- We first try the singletons, if that fails, we move to Top -}
  if null newEvidence
    then do
      io $ putStrLn $ "\nCould not block any singletons"
      io $ putStrLn $ " -> Moving to Top"

      {- update log -}
      updateLog "\ncould not block any singletons\n"
      updateLog "-> moving to top\n"
      
      return Top
    else do
    {- We then try the conjunction of successfuls, if that's successful,
         we return this raised upper bound -}
      successes <- io $ successfulBlockingClauses newEvidence
      io $ putStrLn $ "We could block these clauses:\n"++(show successes)
      let succConj = concat successes
      
      io $ putStrLn "So let's try the pairwise combos."
      updateLog "we successfully blocked the above singleton(s),\nso let's try the pairwise combos\n\n"
      {- Set up instrumentation/logging context -}
      let pairM = 2
      let pairCtx = SearchContext csc succConj prog prefix pfolio d
      let pairP' = pathPrefix pairCtx
      let pairLevelPrefix = pairP' ++ "/" ++ (show pairM)
      let pairCtx' = pairCtx { pathPrefix=pairLevelPrefix }
      io $ createDirectoryIfMissing True pairLevelPrefix
          
      pairLevelElements <- getLevelElements strat pairM pairCtx' {- pairs -}
      let sortedPairs = sortPairs pairLevelElements
      newPairEvidence <- runOnBlocks csc ctx' sortedPairs []
--      pairRs <- runGenPortfolio pairCtx' sortedPairs
--      let pairRs' = concatQuads $ catMaybes pairRs
--      let newPairEvidence = filter (isNewGenEvidence csc) pairRs'
      
      if null newPairEvidence
        then do
          {- If that is not successful, we pick one of the singletons -}
          io $ putStrLn "\nWe couldn't block the pairwise combos..."
          io $ putStrLn " -> Only blocked a singleton"

          {- update log -}
          updateLog "we couldn't block a pairwise combo\n...picking a random singleton\n\n"

          singletonUpper <- io $ pickSubspace newEvidence (runningLog st)
          return singletonUpper
        else do
          {- Otherwise we pick one from the pair evidence -}
          let es = map (\(e,conj,_,t)->(e,conj,t)) newPairEvidence
          let r = makeGenResultLog es
          updateLog r
          
          io $ putStrLn " -> Blocked a PAIR of singletons"
          updateLog "we could block a pair of singletons\n...picking a random pair"
          pairUpper <- io $ pickSubspace newPairEvidence (runningLog st)
          
          {- update log, including all pair evidence, and the one picked -}
          return pairUpper

binarySearch :: GenStrategy -> AcaComputation GeneralizeResult
binarySearch strat = do
  st <- get
  let (AcaState prog csc pfolio d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = st
  let debug = ((d == Full) || (d == Generalize))
  if debug then do io $ putStrLn "** Generalizing **" else return ()
  byPred assert "There is some partition describing reachability"
    (not . null) (disjointPartitions csc) $ return ()
  prefix <- getPathPrefix
  io $ createDirectoryIfMissing True prefix
  let c = lastAddedConjunction csc
  let levels = length c
  let l = 1 -- singletons 
  let r = levels - 1 -- one clause removed
  let lastSuccess = Top
  let context = SearchContext csc c prog prefix pfolio d
  if ((strat == OptimisticEq) || (strat == OptimisticDisEq))
    then binarySearchWorker strat l r lastSuccess context
    else do
      if r >= 2
        then do    
          binarySearchWorker strat 1 2 lastSuccess context {- only explore top 2 levels -}
        else do
          binarySearchWorker strat 1 1 lastSuccess context {- only explore singletons -}       
  

data SearchContext = SearchContext {
  currentCsc              :: Csc,
  conjunctionToGeneralize :: Conjunction,
  searchProgram           :: Program,
  pathPrefix              :: String,
  searchPortfolio         :: Portfolio,
  searchDebug             :: DebugMode
  }
  
binarySearchWorker :: GenStrategy -> Int -> Int -> GeneralizeResult -> SearchContext -> AcaComputation GeneralizeResult
binarySearchWorker strat l r lastSuccess ctx = do
  let debug = (((searchDebug ctx) == Full) || ((searchDebug ctx) == Generalize))
  if (l > r)
    then do
      if debug
        then do io $ putStrLn $ "Binary search finished with: " ++ (show lastSuccess)
        else return ()
      return lastSuccess
    else do
      let m = (l + r) `div` 2
      if debug
        then do io $ putStrLn $ "Binary search at lattice level: " ++ (show m)
        else return ()

      let p' = pathPrefix ctx
      let levelPrefix = p' ++ "/" ++ (show m)
      let ctx' = ctx { pathPrefix=levelPrefix }
      io $ createDirectoryIfMissing True levelPrefix
      
      levelElements <- getLevelElements strat m ctx {- limited to a width of 10 -}
      rs <- runGenPortfolio ctx' levelElements
      let rs' = concatQuads $ catMaybes rs
      
      csc <- getCsc
      let newEvidence = filter (isNewGenEvidence csc) rs'
      if null newEvidence
        then do
          let r' = m - 1
          binarySearchWorker strat l r' lastSuccess ctx
        else do
          st <- get
          let logH = runningLog st
          lastSuccess' <- io $ pickSubspace newEvidence logH
          let l' = m + 1
          binarySearchWorker strat l' r lastSuccess' ctx

type LatticeElement = (Int, [Conjunct])

runGenPortfolio :: SearchContext -> [LatticeElement] -> AcaComputation [(Maybe (EvidenceCollection, Conjunction, Csc, Float))] 
runGenPortfolio ctx levelElements = do
  configs <- mapM (\(tag, _) -> (setupRunConfiguration (Just tag))) levelElements
  let elemConfigTuples = zip configs levelElements
  ef <- getExitSummaryFile
  p <- getParallelism
  if p == InParallel
    then do
      rs <- io $ (mapConcurrently (runOnLatticeElement ctx) elemConfigTuples) `onException` (writeExitSummary ef "Generalization")
      return rs
    else do
      rs <- io $ mapM (runOnLatticeElement ctx) elemConfigTuples
      return rs

concatQuads :: [([a],b,c,d)] -> [(a, b, c, d)]
concatQuads xs =
  let tuplesLists = map (\(ys, conj, csc, t) -> zip4 ys (repeat conj) (repeat csc) (repeat t)) xs
  in concat tuplesLists

isNewGenEvidence :: Csc -> (PieceOfEvidence, Conjunction, Csc, Float) -> Bool
isNewGenEvidence _ ((LegitimateEvidence (AnalysisWitness _ _ _ True _ _ _) _ _), _, _, _) = True
isNewGenEvidence _ ((SpuriousEvidence _ _ _), _, _, _) = False
isNewGenEvidence csc (e, _, _, _) =
  let subspace = (conjunction . characterization) e
      lowerBounds = concat $ map (lower . lowerBound) (disjointPartitions csc)
  in not $ subspace `elem` lowerBounds

isSafeOrFailing :: PieceOfEvidence -> Bool
isSafeOrFailing (SpuriousEvidence _ _ _) = False
isSafeOrFailing e = 
  ((isSafe $ analysisWitness e) || ((not . null . conjunction . characterization) e))

runOnLatticeElement :: SearchContext -> (RunConfiguration, LatticeElement) -> IO (Maybe (EvidenceCollection, Conjunction, Csc, Float))
runOnLatticeElement ctx@(SearchContext _ _ _ prefix _ _) (runConfig, el@(tag, _)) = do
  -- create element directory
  let elementDir = prefix ++ "/" ++ (show tag)
  createDirectoryIfMissing True elementDir

  start <- getCurrentTime
  (program, csc) <- instrumentLatticeElem ctx el
  end <- getCurrentTime
  let tentativeWidenTime = (realToFrac $ diffUTCTime end start)::Float
  
  elementResults <- exploreGenSubspace program ctx tag runConfig
  let conj = snd el
  if isJust elementResults
    then do
      let (Just collection) = elementResults
      return (Just (collection, conj, csc, tentativeWidenTime))
    else return Nothing
  
exploreGenSubspace :: Program -> SearchContext -> Int -> RunConfiguration -> IO (Maybe EvidenceCollection)
exploreGenSubspace program (SearchContext csc _ _ _ pfolio debugMode) _ runConfig = do
  let debug = ((debugMode == Full) || (debugMode == Generalize))
  
  results <- runPortfolio program pfolio csc runConfig
  
  if debug
    then do putStrLn $ "\nGeneralization results: " ++ (show results)
    else return ()
    
  if debug
    then do putStrLn $ "\nCIVL-checked generalization results: " ++ (show results)
    else return ()
  
  let newResults = filter (isNewEvidence csc) results
  let newEvidence = filter isSafeOrFailing newResults
  if null newEvidence
    then return Nothing
    else return (Just newEvidence)

instrumentLatticeElem :: SearchContext -> LatticeElement -> IO (Program, Csc)
instrumentLatticeElem (SearchContext prevCsc _ p pathPre _ _) (i, conj) = do
  csc <- makeTentativeCsc prevCsc conj
  let (Program _ a n _ _) = p
      ast' = updateTransform a csc
      progStr = show $ pretty ast'
      filePath = pathPre ++ "/" ++ (show i) ++ "/" ++ n ++ "." ++ (show i) ++ ".c"
      debug = False
  if debug then do putStrLn $ "Update:\n\n " ++ progStr else do return ()
  writeFile filePath progStr
  let program' = (Program filePath ast' n i (pathPre++"/"))
  return (program', csc)

makeTentativeCsc :: Csc -> Conjunction -> IO Csc
makeTentativeCsc csc conj = do
  csc' <- tentativeWiden csc conj
  {- update log -}
  return csc'
  
pickSubspace :: [(PieceOfEvidence, Conjunction, Csc, Float)] -> FilePath -> IO GeneralizeResult
pickSubspace l@[] _ = byPred assert "There is some subspace to pick from." (\_ -> False) l $ return Top
pickSubspace es l = do
  -- filter out SpuriousEvidence
  let es' = filter isLegitimateTuple es
      safes = filter (isSafe . analysisWitness . extractFirst) es'
  -- first find any safety claims, then look for evidence
  -- using an arbitrary selection
  if (not $ null safes)
    then do
      let safeC = head $ reverse $ sortBy (\(_,c1,_,_) (_,c2,_,_)->(sort c1) `compare` (sort c2)) safes
      let (e1,c,csc,_) = safeC
      appendFile l $ "after generalizing to the formula:\n "++(showMinimalConjunction c)++"\nthe remainder of the program cannot reach psi\n...evidence used:\n"
      let rs = makeResultLog [e1]
      appendFile l rs
      return $ SafetySpace csc
    else do
      putStrLn "These are the conjunctions that were successfully blocked:"
      mapM_ (\(_,c,_,_) -> putStrLn $ show c) es'
      let (e, form, csc,_) = head es'
      appendFile l $ "after generalizing to the formula:\n "++(showMinimalConjunction form)++"\na new reachability condition has been found:\n"
      let rs = makeResultLog [e]
      appendFile l rs
      return $ NewSpace csc (characterization e)--Right $ (characterization . fst . head) es'

successfulBlockingClauses :: [(PieceOfEvidence, Conjunction, Csc, Float)] -> IO [Conjunction]
successfulBlockingClauses es = do
  let es' = filter isLegitimateTuple es
--      ns = filter (not . isSafe . analysisWitness . extractFirst) es'
      successfulConjs = map (\(_,c,_,_) -> c) es'
  return successfulConjs

extractFirst :: (a, b, c, d) -> a
extractFirst (a,_,_,_) = a
  
isLegitimateTuple :: (PieceOfEvidence, Conjunction, Csc, Float) -> Bool
isLegitimateTuple ((SpuriousEvidence _ _ _), _, _, _) = False
isLegitimateTuple _ = True

sortPairs :: [LatticeElement] -> [LatticeElement]
sortPairs es =
  let es' = map (\(i, cs) -> (i, (reverse $ sort $ cs))) es
      es'' = reverse $ sortBy (\(_, cs1) (_, cs2) -> cs1 `compare` cs2) es'
  in es''

getLevelElements :: GenStrategy-> Int -> SearchContext -> AcaComputation [LatticeElement]
getLevelElements OptimisticEq l (SearchContext _ conj _ _ _ _) = do
  let conjunctions = reverse $ sort $ combinations l conj
      widthLimit   = 10
  if ((length conjunctions) > widthLimit)
    then do
      return $ zip [0..] (take widthLimit conjunctions) -- limit lattice width
    else do
      return $ zip [0..] conjunctions
getLevelElements OptimisticDisEq l (SearchContext _ conj _ _ _ _) = do
  let conjunctions = sort $ combinations l conj
      widthLimit   = 10
  if ((length conjunctions) > widthLimit)
    then do
      return $ zip [0..] (take widthLimit conjunctions) -- limit lattice width
    else do
      return $ zip [0..] conjunctions
getLevelElements PessimisticEq l (SearchContext _ conj _ _ _ _) = do
  {- Either singletons or pairs -}
  let conjunctions = reverse $ sort $ combinations l conj
  return $ zip [0..] conjunctions
getLevelElements PessimisticDisEq l (SearchContext _ conj _ _ _ _) = do
  {- Either singletons or pairs -}
  let conjunctions = sort $ combinations l conj
  return $ zip [0..] conjunctions
  
-- solution taken from:
-- stackoverflow.com/questions/43229580/combinations-of-k-elements-from-set-n-haskell
combinations :: (Eq a, Num a) => a -> [t] -> [[t]] 
combinations 0 _ = [[]]
combinations n ls = [ (x:ys) | (x:xs) <- tails ls, ys <- combinations (n-1) xs ]

getPathPrefix :: AcaComputation String
getPathPrefix = do
  st <- get
  let prog  = stateProgram st
  let iter  = iteration prog
  let pName' = pName prog
  logPre <- getLogPrefix
  return $ logPre++"/logs_alpaca/"++pName'++"/iter."++(show iter)++"/generalization/lattice_level"

removeLastDisjointPartition :: Csc -> Csc 
removeLastDisjointPartition (Csc disjParts inMap d s f) = Csc (init disjParts) inMap d s f

{- This is near-redundant code from Solver.hs; it should prolly be refactored -}
tentativeWiden :: Csc -> Conjunction -> IO Csc
tentativeWiden csc generalClause = do

  let uppers = upperBounds csc

  {- Intersections -}
  inResults <- mapM (intersectsIO generalClause) (zip [1..] uppers)
  let intersections = map fst $ filter isSatRes inResults
      unknowns1 = map fst $ filter isUnknownRes inResults

  {- Implications -}
  imResults <- mapM (impliesIO generalClause) (zip [1..] intersections)

  let subsumed = map fst $ filter isProven imResults
      partials = map fst $ filter hasCounterExample imResults
      unknowns2 = map fst $ filter proofUnknown imResults

      excludedFormulae = partials ++ unknowns1 ++ unknowns2
      safeGeneralClause = UpperBound generalClause excludedFormulae
      
      {- Collect the children of the subsumed -}
      subsumptions = map (findLowerBound csc) subsumed
      lbConjs = concat $ map lower subsumptions
      nSlices = sum $ map nSliced subsumptions
      subsumptions' = LowerBound lbConjs nSlices

      assumes = concat $ map (findAssumptions csc) subsumed
      partition = CscPartition safeGeneralClause subsumptions' assumes
      csc' = csc `removeSubsumed` subsumed
      csc'' = csc' `addPartition` partition
  return csc''

impliesIO :: Conjunction -> (Int, Conjunction) -> IO (Conjunction, ThmResult)
impliesIO up (_, c1) = do
  let antecedent = makeConjunctionExpr $ map (\(Conjunct c)->c) c1
      consequent = makeConjunctionExpr $ map (\(Conjunct c)->c) up
      aNames = map extractVarName $ extractCIndexes antecedent
      cNames = map extractVarName $ extractCIndexes consequent
      env = nub (aNames ++ cNames)
  result <- impliesWorker antecedent consequent env
  return (c1, result)

intersectsIO :: Conjunction -> (Int, Conjunction) -> IO (Conjunction, SatResult)
intersectsIO gen (_, up) = do
  let gen' = makeConjunctionExpr $ map (\(Conjunct c)->c) gen
      up' = makeConjunctionExpr $ map (\(Conjunct c)->c) up
      genAndUp = makeConjunctionExpr [up',gen']
      env = nub $ map extractVarName $ extractCIndexes genAndUp
  result <- isSat genAndUp env
  return (up, result)
\end{code}
