module Octagon where

import qualified Data.Map.Strict as M
import Language.C
import Language.C.Data.Ident
import Data.List (sort, nub)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.SBV
import Data.SBV.Internals hiding (LessEq)
import CscTypes
import SolverLib

{- Port of Vu's 'generalize' Python script -}

data Sign = Pos | Neg deriving (Eq)
instance Show Sign where
  show Pos = "+"
  show Neg = "-"

data Variable = Var String Sign | Zero deriving (Eq)
instance Show Variable where
  show Zero = "v0"
  show (Var vName sign) = (show sign)++vName

type Bound = Int
data OctConstraint = OctConstraint SignedOctForm Bound deriving (Show)

data SignedOctForm = SignedOneVar Variable
                   | SignedTwoVar Variable Variable
                   deriving (Show)

toSignedOct :: OctForm -> SignedOctForm
toSignedOct (OctOneVar v) = SignedOneVar (toVariable v)
toSignedOct (OctTwoVar v1 v2) = SignedTwoVar (toVariable v1) (toVariable v2)

toVariable :: OctTuple -> Variable
toVariable (1, varName) = Var varName Pos
toVariable (-1, varName) = Var varName Neg
toVariable o = error $ "Unexpected oct form given: "++(show o)

octagonConstraints :: Int -> Int -> Conjunction -> IO [OctConstraint]
octagonConstraints up low cs = do
  let c = makeConjunctionExpr $ map (\(Conjunct con)->con) cs
  let env = nub $ map extractVarName $ extractCIndexes c
  putStrLn "Generalizing the formula:"
  putStrLn (show $ pretty c)
  let os = getOctForms env
  let ubs = map (mkUbExpr up) os
  rs <- mapM (\ub -> check c ub env) ubs
  let rs' = zip os rs
  let rs'' = filter (\(_,r) -> (not . disproved) r) rs'
  ubVs <- mapM ((_f c low up env) . fst) rs''
  let ubVs' = zip (map fst rs'') ubVs 
  let ubVs'' = catMaybes $ map dropTuplesWithoutBound ubVs'
  let octCs = map (\(o,i)->OctConstraint (toSignedOct o) i) ubVs''
  return octCs

{- Turn this into octagonConjunction -}
octagonalize :: Conjunction -> Int -> Int -> IO Conjunction
octagonalize cs up low = do
  let c = makeConjunctionExpr $ map (\(Conjunct con)->con) cs
  let env = nub $ map extractVarName $ extractCIndexes c
  putStrLn "Generalizing the formula:"
  putStrLn (show $ pretty c)
  let os = getOctForms env
  let ubs = map (mkUbExpr up) os
  rs <- mapM (\ub -> check c ub env) ubs
  let rs' = zip os rs
  let rs'' = filter (\(_,r) -> (not . disproved) r) rs'
  ubVs <- mapM ((_f c low up env) . fst) rs''
  let ubVs' = zip (map fst rs'') ubVs 
  let ubVs'' = catMaybes $ map dropTuplesWithoutBound ubVs'
  let ps = map (\(o,i)-> Conjunct $ mkUbExpr i o) ubVs''
  putStrLn "Octagon constraints:"
  mapM_ (putStrLn . show) ps
  return ps

dropTuplesWithoutBound :: (OctForm, Maybe Int) -> Maybe (OctForm, Int)
dropTuplesWithoutBound (_, Nothing) = Nothing
dropTuplesWithoutBound (o, Just v) = Just (o,v)

data SolverResult = SolverProved | SolverDisproved | SolverUnknown deriving (Show, Eq)

type CExMap = M.Map Var CV

extractSolverRes :: ThmResult -> SolverResult
extractSolverRes (ThmResult (Unsatisfiable _ _)) = SolverProved
extractSolverRes (ThmResult (Satisfiable _ _)) = SolverDisproved
extractSolverRes _ = SolverUnknown
  
existsAndIsDisproved :: Maybe SolverResult -> Bool
existsAndIsDisproved Nothing = False
existsAndIsDisproved (Just SolverDisproved) = True
existsAndIsDisproved _ = False

getOctForms :: [Var] -> [OctForm]
getOctForms vs =
  let oneVars = concat $ map (\v->map (\i->OctOneVar (i,v)) [1,-1]) vs
      twoVarPairs = [[(i1,v1), (i2,v2)] | i1<-[-1,1], i2<-[-1,1], v1<-vs, v2<-vs, v1 /= v2]
      twoVarPairs' = nub $ map sort twoVarPairs
      twoVars = map (\[vt1, vt2]->OctTwoVar vt1 vt2) twoVarPairs'
  in oneVars++twoVars

disproved :: ThmResult -> Bool
disproved (ThmResult (Satisfiable _ _)) = True
disproved _ = False

check :: CExpr -> CExpr -> [Var] -> IO ThmResult
check f g vs = impliesWorker f g vs

mkUbExpr :: Int -> OctForm -> CExpr
mkUbExpr up (OctOneVar t) = CBinary CLeqOp (mkExpr t) (mkConst up) undefNode
mkUbExpr up (OctTwoVar t1 t2) = CBinary CLeqOp (CBinary CAddOp (mkExpr t1) (mkExpr t2) undefNode) (mkConst up) undefNode

mkConst :: Int -> CExpr
mkConst i = (CConst (CIntConst (cInteger (toInteger i)) undefNode))

mkExpr :: OctTuple -> CExpr
mkExpr (-1,v) = CUnary CMinOp (makeCIndex v) undefNode
mkExpr (1,v) = makeCIndex v
mkExpr t = error $ "I don't know how to handle this octagon tuple: "++(show t)

makeCIndex :: String -> CExpr
makeCIndex s =
  let tokens = splitOn "[" s
      ident = oMakeIdent (head tokens)
      idx = init (head $ tail $ tokens)
      idxInt = read idx :: Int
      idxConst = mkConst idxInt
  in CIndex (CVar ident undefNode) idxConst undefNode

oMakeIdent :: String -> Ident
oMakeIdent s = Ident s 0 undefNode
  
type OctTuple = (Int, Var)

data OctForm = OctOneVar OctTuple
             | OctTwoVar OctTuple OctTuple
             deriving (Show, Eq)
  
type Var = String

_f :: CExpr -> Int -> Int -> [Var] -> OctForm -> IO (Maybe Int)
_f c minV maxV env o = do
  let statsd = M.singleton maxV SolverProved
  (maybeBoundV, statsd') <- gc c o minV maxV statsd env
  case maybeBoundV of
    Just boundV -> do
      case M.lookup boundV statsd' of
        Nothing -> do
          return (Just boundV)
        Just entry ->
          if entry /= SolverDisproved
            then do
              return (Just boundV)
            else return Nothing
    Nothing -> return Nothing

gc :: CExpr -> OctForm -> Int -> Int -> M.Map Int SolverResult -> [Var] -> IO (Maybe Int, M.Map Int SolverResult)
gc c o minV maxV statsd env = do
  if minV == maxV
    then return (Just minV, statsd)
    else do
      if ((maxV - minV) == 1)
        then do
          let v = M.lookup minV statsd
          if (existsAndIsDisproved v)
            then return (Just maxV, statsd)
            else do
              {- lines 49--60 of generalize.py -}
              let inv = mkUbExpr minV o
              stat <- check c inv env
              let statRes = extractSolverRes stat
              let statsd' = M.insert minV statRes statsd
    
              if statRes == SolverDisproved
                then return (Just maxV, statsd')
                else return (Just minV, statsd')
        else do
          {- lines 62--76 of generalize.py -}
          let v = ceiling (((fromIntegral :: (Integral a) => a -> Double) (maxV + minV)) / 2.0)
          let inv = mkUbExpr v o
          stat <- check c inv env
          let statRes = extractSolverRes stat
          let statsd' = M.insert minV statRes statsd
          if statRes == SolverDisproved
            then do
              let tcs = meval o stat
              gc c o tcs maxV statsd' env
            else do
              let maxV' = v
              gc c o minV maxV' statsd' env

{- This is only called if there are counter examples -}
meval :: OctForm -> ThmResult -> Int
meval o (ThmResult (Satisfiable _ (SMTModel _ vals))) =
  let m = M.fromList vals
  in produceMeval o m
meval _ _ = error "Expecting a model to be given."

produceMeval :: OctForm -> CExMap -> Int
produceMeval (OctOneVar (i, v)) m = 
  let (Just val) = M.lookup v m
  in (fromIntegral ((toInteger i)*(fromCV val)::Integer))
produceMeval (OctTwoVar (i1, v1) (i2, v2)) m =
  let (Just val1) = M.lookup v1 m
      (Just val2) = M.lookup v2 m
  in (fromIntegral (((toInteger i1)*(fromCV val1)) + ((toInteger i2)*(fromCV val2))))
