module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Map.Strict as Map (fromList, Map, lookup, size, assocs, toList, fromListWith)
import Data.Matrix hiding (fromList, toList)
import Data.Ord (comparing)
import Data.List hiding (lookup)
import Data.List.Split (splitOn)
import Data.Maybe
import Control.Monad.State
import System.Process
import Language.C hiding (execParser) -- simple API
import Language.C.System.GCC   -- preprocessor used
import Language.C.Data.Ident
import Prelude hiding (lookup)

main :: IO ()
main = do
  runMain =<< execParser opts
  return ()
      
runMain :: Configuration -> IO ()
runMain (Configuration program cThresh dThresh) = do
  cs <- calledFuncs program cThresh
  ds <- deepFuncs program dThresh
  let candidates = intersect cs ds
  mapM_ putStrLn candidates
  return ()

opts :: ParserInfo Configuration
opts = info ( config <**> helper )
  (  fullDesc
  <> header   "candidatefunctions -- List the functions in a C file that are called \
              \ from some number of sites in some conditional context (e.g., within \
              \ the body of an 'if' statement.)" )

data Configuration = Configuration {
  prog :: FilePath,
  callSiteFreq :: Int,
  condDepth :: Int
}

config :: Parser Configuration
config = Configuration
  <$> argument str
      (  metavar "C_FILE" )
  <*> option auto
      (  long "minCallSites"
      <> help "minimum number of different call sites"
      <> showDefault
      <> value 2
      <> metavar "INT" )
  <*> option auto
      (  long "minCondDepth"
      <> help "minimum conditional context depth"
      <> showDefault
      <> value 2
      <> metavar "INT" )

type Depth = Int

deepFuncs :: FilePath -> Depth -> IO [FuncName]
deepFuncs program dThresh = do
  ast <- getAst program
  let fs = collectFunctions ast
      localRelations = map processFunction fs
      functionRelations = zip fs localRelations
      minFunctionRelations = map minimize functionRelations
      nameIdMap = makeNameToIdMap minFunctionRelations
      dim = size nameIdMap
      edges = makeAllEdges minFunctionRelations
      closureMatrix = transitiveClosure dim nameIdMap edges
      depthTuples = nestingTuples closureMatrix nameIdMap
      depthTuples' = filter (\(_,d) -> aboveDepthThresh d dThresh) depthTuples
      funcNames = map fst depthTuples'
  return funcNames

calledFuncs :: FilePath -> Thresh -> IO [FuncName]
calledFuncs f t = do
  cflowOut <- cflow f
  let freqTable = makeFunctionFrequencyTable cflowOut
  let sortedFreqTable = sortBy (\(_,a) (_,b) -> compare b a) freqTable
  let candidatePairs = filter (\(_,freq) -> freq >= t) sortedFreqTable
  let candidateFunctions = map fst candidatePairs
  return candidateFunctions

cflow :: FilePath -> IO String
cflow f = do
  readProcess "cflow" ["-x", f] []

{- https://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs -}
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])
  
makeFunctionFrequencyTable :: String -> [(FuncName, Int)]
makeFunctionFrequencyTable stdout =
  let ls = lines stdout
      ls' = map (\y -> head ((splitOn " ") y)) ls
      freqList = frequency ls'
  in freqList

type Thresh = Int

aboveDepthThresh :: Distance -> Thresh -> Bool
aboveDepthThresh Infinity _ = False
aboveDepthThresh (Number n) t = n >= t

type FuncName = String

nestingTuples :: Matrix Distance -> Map.Map String Int -> [(FuncName, Distance)]
nestingTuples mt nameIdMap =
  let (Just mainIndex) = lookup "main" nameIdMap
      mainRow = (toLists mt) !! (mainIndex - 1) {- 1-indexing to 0-indexing -}
      mainRowIndices = zip [1..] mainRow
      idNameMap = reverseMap nameIdMap
      tuples = map (\e->makeNestingTuple e idNameMap) mainRowIndices
  in tuples

makeNestingTuple :: (Int, Distance) -> Map.Map Int String -> (FuncName, Distance)
makeNestingTuple (i, callDepth) mp =
  let (Just functionName) = lookup i mp
  in (functionName, callDepth)

type Dimension = Int

-- Following pseudocode from wiki on Floyd-Warshall
transitiveClosure :: Dimension -> Map.Map String Int -> [Edge] -> Matrix Distance
transitiveClosure dim nameIdMap es =
--let dist be a |V| × |V| array of minimum distances initialized to ∞ (infinity)
  let mat = matrix dim dim (\(_,_)->Infinity)
--for each edge (u,v)
--   dist[u][v] ← w(u,v)  // the weight of the edge (u,v)
      mat' = foldr (\e@(Edge _ _ w) -> setElem (Number w) (getIndex e nameIdMap)) mat es
--for each vertex v
--   dist[v][v] ← 0
      mat'' = foldr (\i->setElem (Number 0) (i,i)) mat' [1..dim]
--for k from 1 to |V|
--   for i from 1 to |V|
--      for j from 1 to |V|
--         if dist[i][j] > dist[i][k] + dist[k][j] 
--            dist[i][j] ← dist[i][k] + dist[k][j]
--         end if
      indices = [(k,i,j)|k<-[1..dim],i<-[1..dim],j<-[1..dim]]
      mat''' = foldr (\(k,i,j)->updateMatrixElement k i j) mat'' indices
  in mat'''

updateMatrixElement :: Int -> Int -> Int -> Matrix Distance -> Matrix Distance
updateMatrixElement k i j mat =
  let d_i_j = getElem i j mat
      d_i_k = getElem i k mat 
      d_k_j = getElem k j mat
  in
    if d_i_j > (d_i_k +++ d_k_j)
      then setElem (d_i_k +++ d_k_j) (i,j) mat
      else mat

(+++) :: Distance -> Distance -> Distance
(+++) Infinity _ = Infinity
(+++) _ Infinity = Infinity
(+++) (Number a) (Number b) = Number (a + b)

getIndex :: Edge -> Map.Map String Int -> (Int, Int)
getIndex (Edge callerName calleeName _) m =
  let (Just callerId) = lookup callerName m
      (Just calleeId) = lookup calleeName m
  in (callerId, calleeId)
  
data Distance = Infinity | Number Int deriving (Eq)
instance Ord Distance where
  Infinity `compare` (Number _) = GT
  (Number _) `compare` Infinity = LT
  Infinity `compare` Infinity = EQ
  (Number a) `compare` (Number b) = a `compare` b
instance Show Distance where
  show Infinity = "∞"
  show (Number a) = show a

type Callee = String
type Scope = Int
type ScopeCtx a = State Scope a

type FunctionRelation = (Function, [(Callee, Scope)])

data Edge = Edge {
  caller :: String,
  callee :: String,
  weight :: Int
}

makeNameToIdMap :: [FunctionRelation] -> Map.Map String Int
makeNameToIdMap fs =
  -- grab unique function names
  let funcs = concat $ map (\((Function n1 _), rs)->
                       [n1]++map (\(n2,_)->n2) rs) fs
      uniqueFunc = nub funcs
      tuples = zip uniqueFunc [1..]
  in Map.fromList tuples

reverseMap :: (Ord b) => Map.Map a b -> Map.Map b a
reverseMap m =
  let aMap = assocs m
      aMapRev = map (\(a,b)->(b,a)) aMap
  in fromList aMapRev

makeFunctionEdges :: FunctionRelation -> [Edge]
makeFunctionEdges ((Function n _), rs)  = map (\(c,s)->Edge n c s) rs

makeAllEdges :: [FunctionRelation] -> [Edge]
makeAllEdges fs = concat $ map makeFunctionEdges fs
  
minimize :: FunctionRelation -> FunctionRelation
minimize (f, rs) =
  let rs' = sortOn fst rs
      groups = groupBy (\(a,_) (b,_) -> a==b) rs'
      minRelations = map (minimumBy (comparing snd)) groups
  in (f, minRelations)
      
processFunction :: Function -> [(Callee, Scope)]
processFunction (Function _ ss) =
  let maybeRelations = map (\b -> evalState (processBlockItem b) 0) ss
      relations = catMaybes maybeRelations
  in (concat relations)

type ProcessResult = Maybe [(Callee, Scope)]

processBlockItem :: CBlockItem -> ScopeCtx ProcessResult
processBlockItem (CBlockStmt s) = processStatement s
processBlockItem (CBlockDecl d) = processLocalDecl d
processBlockItem _ = return Nothing

incrementScope :: ScopeCtx ()
incrementScope = do { x <- get; put (x+1); return () }

decrementScope :: ScopeCtx ()
decrementScope = do { x <- get; put (x-1); return () }
  
processStatement :: CStat -> ScopeCtx ProcessResult
processStatement (CLabel _ s _ _) = processStatement s
processStatement (CCase e s _) = do
  eRes <- processExpression e
  sRes <- processStatement s
  return (flattenResults [eRes, sRes])
processStatement (CCases e1 e2 s _) = do
  e1Res <- processExpression e1
  e2Res <- processExpression e2  
  sRes <- processStatement s
  return (flattenResults [e1Res, e2Res, sRes])
processStatement (CDefault s _) = processStatement s
processStatement (CExpr (Just e) _) = processExpression e
processStatement (CCompound _ bs _) = do
  rs <- mapM processBlockItem bs
  return (flattenResults rs)
processStatement (CIf cond@(CConst _) thenExpr Nothing _) = do
  cRes <- processExpression cond
  tRes <- processStatement thenExpr
  return (flattenResults [cRes, tRes])
processStatement (CIf cond thenExpr Nothing _) = do
  cRes <- processExpression cond
  incrementScope
  tRes <- processStatement thenExpr
  decrementScope
  return (flattenResults [cRes, tRes])
processStatement (CIf cond thenExpr (Just elseExpr) _) = do
  cRes <- processExpression cond
  incrementScope
  tRes <- processStatement thenExpr
  eRes <- processStatement elseExpr
  decrementScope
  return (flattenResults [cRes, tRes, eRes])
processStatement (CSwitch e@(CConst _) s _) = do
  eRes <- processExpression e
  sRes <- processStatement s
  return (flattenResults [eRes, sRes])
processStatement (CSwitch e s _) = do
  eRes <- processExpression e
  incrementScope
  sRes <- processStatement s
  decrementScope
  return (flattenResults [eRes, sRes])
processStatement (CWhile e@(CConst _) s _ _) = do
  eRes <- processExpression e
  sRes <- processStatement s
  return (flattenResults [eRes, sRes])
processStatement (CWhile e s _ _) = do
  eRes <- processExpression e
  incrementScope
  sRes <- processStatement s
  decrementScope
  return (flattenResults [eRes, sRes])
processStatement (CFor (Left (Just expr1)) mExpr2 mExpr3 s _) = do
  e1 <- processExpression expr1
  e2 <- processMaybeExpression mExpr2
  e3 <- processMaybeExpression mExpr3
  incrementScope
  sRes <- processStatement s
  decrementScope
  return (flattenResults ([e1]++e2++e3++[sRes]))
processStatement (CFor (Right d) mExpr1 mExpr2 s _) = do
  dRes <- processLocalDecl d
  e1 <- processMaybeExpression mExpr1
  e2 <- processMaybeExpression mExpr2
  incrementScope
  sRes <- processStatement s
  decrementScope
  return (flattenResults ([dRes]++e1++e2++[sRes]))
processStatement (CGotoPtr e _) = processExpression e
processStatement (CReturn (Just e) _) = processExpression e
processStatement _ = return Nothing

processMaybeExpression :: Maybe CExpr -> ScopeCtx [ProcessResult]
processMaybeExpression Nothing = return []
processMaybeExpression (Just e) = do
  r <- processExpression e
  return [r]

processExpression :: CExpr -> ScopeCtx ProcessResult
processExpression (CComma es _) = do
  rs <- mapM processExpression es
  return (flattenResults rs)
processExpression (CAssign _ _ rhs _) = processExpression rhs
processExpression (CCond c (Just e1) e2 _) = do
  cond <- processExpression c
  incrementScope
  trueBranch <- processExpression e1
  falseBranch <- processExpression e2
  decrementScope
  return (flattenResults [cond,trueBranch,falseBranch])
processExpression (CBinary _ lOp rOp _) = do
  lRes <- processExpression lOp
  rRes <- processExpression rOp
  return (flattenResults [lRes,rRes])
processExpression (CCast _ e _) = processExpression e
processExpression (CUnary _ e _) = processExpression e
processExpression (CSizeofExpr e _) = processExpression e
processExpression (CAlignofExpr e _) = processExpression e
processExpression (CComplexReal e _) = processExpression e
processExpression (CComplexImag e _) = processExpression e
processExpression (CIndex _ e _) = processExpression e
processExpression (CCall (CVar (Ident n _ _) _) _ _) = do
  scope <- get
  return (Just [(n, scope)])
processExpression _ = return Nothing

flattenResults :: [ProcessResult] -> ProcessResult
flattenResults rs = 
  let rs' = concat $ catMaybes rs
  in
    if null rs'
      then Nothing
      else (Just rs')

processLocalDecl :: CDecl -> ScopeCtx ProcessResult
processLocalDecl (CDecl _ decls _) = do
  let initExprs = map (\(_, (Just (CInitExpr e _)), _)->e) (filter hasInitExpr decls)
  rs <- mapM processExpression initExprs
  return (flattenResults rs)
processLocalDecl _ = return Nothing

type DeclBody = (Maybe CDeclr, Maybe CInit, Maybe CExpr)

hasInitExpr :: DeclBody -> Bool
hasInitExpr (_, (Just (CInitExpr _ _)), _) = True
hasInitExpr _ = False

data Function = Function {
  name :: String,
  body :: [CBlockItem]
  } deriving (Eq)
instance Show Function where
  show (Function n _) = show n
instance Ord Function where
  (Function n1 _) `compare` (Function n2 _) = n1 `compare` n2

collectFunctions :: CTranslUnit -> [Function]
collectFunctions (CTranslUnit es _) =
  let fs = map (\(CFDefExt f)->f) $ filter isFunction es
      bodies = map (\(CFunDef _ _ _ b _)->b) fs
      bodies' = map (\(CCompound _ b _)->b) bodies
      names = map extractName fs
      functions = map (\(n,b) -> Function n b) (zip names bodies')
  in functions

isFunction :: CExtDecl -> Bool
isFunction (CFDefExt (CFunDef _ _ _ _ _)) = True
isFunction _ = False

extractName :: CFunDef -> String
extractName (CFunDef _ (CDeclr (Just (Ident n _ _)) _ _ _ _) _ _ _) = n
extractName _ = "UNKNOWN_FUNCTION_NAME"

getAst :: FilePath -> IO CTranslUnit
getAst p = parseCFile (newGCC "gcc") Nothing [] p >>= checkResult "[parsing]"

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return
