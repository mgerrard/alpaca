\begin{code}
module SolverLib where

import CscTypes
import Language.C
import Language.C.Data.Ident
import Data.SBV
import Data.List
import qualified Data.Map.Strict as Map

makeConjunctionExpr :: [CExpr] -> CExpr
makeConjunctionExpr cs = foldr1 (\x y->CBinary CLndOp x y undefNode) cs

makeDisjunctionExpr :: [CExpr] -> CExpr
makeDisjunctionExpr cs = foldr1 (\x y->CBinary CLorOp x y undefNode) cs

disjunctionEqualsTrue :: CExpr -> [String] -> IO ThmResult
disjunctionEqualsTrue d vs = prove equivalence
  where
    equivalence :: Predicate
    equivalence = do
      syms <- mapM forall vs
      let env = Map.fromList (zip vs syms)
      disjunction <- interpret env d
      return $ disjunction .== sTrue

type Env = Map.Map String SInteger

envLookup :: String -> Env -> SInteger
envLookup v e = maybe (error $ "Var not found: " ++ show v) id
                            (Map.lookup v e)

impliesWorker :: CExpr -> CExpr -> [String] -> IO ThmResult
impliesWorker a c vs = prove implication
  where
    implication :: Predicate
    implication = do
      syms <- mapM forall vs
      let env = Map.fromList (zip vs syms)
      antecedent <- interpret env a
      consequent <- interpret env c
      return $ antecedent .=> consequent

makePredicate :: CExpr -> Predicate
makePredicate expr = do
  let vs = nub $ map extractVarName $ extractCIndexes expr
  syms <- mapM exists vs
  let env = Map.fromList (zip vs syms)
  interpret env expr

isSat :: CExpr -> [String] -> IO SatResult
isSat e0 vs = sat pr
 where
  pr :: Predicate
  pr = do
      syms <- mapM exists vs
      let env = Map.fromList (zip vs syms)
      interpret env e0

interpret :: Env -> CExpr -> Predicate
interpret _ (CConst c) = truthValue c
interpret env (CUnary CNegOp expr _) = do
  e <- interpret env expr
  return $ sNot e
interpret env (CBinary op lhs rhs _)
  | isCmpOp op = do
      let l = deriveNumericExpr env lhs
      let r = deriveNumericExpr env rhs
      return $ (sbvCompOp op) l r    
  | isLogicOp op = do
      l <- interpret env lhs
      r <- interpret env rhs
      return $ (sbvLogicOp op) l r
interpret _ e = error $ "This expression type is not implemented: "++(show $ pretty e)

isCmpOp :: CBinaryOp -> Bool
isCmpOp op = op `elem` [ CLeqOp, CGeqOp, CLeOp, CGrOp, CEqOp, CNeqOp ]

isLogicOp :: CBinaryOp -> Bool
isLogicOp op = op `elem` [ CLndOp, CLorOp ]

deriveNumericExpr :: Env -> CExpr -> SInteger
deriveNumericExpr _ (CConst c) = literal (unpackedConst c)
deriveNumericExpr env (CIndex (CVar iden _) (CConst (CIntConst c _)) _) =
  let const' = getCInteger c
      idxStr = show const'
      idStr = identToString iden
      arrElem = idStr++"["++idxStr++"]"
  in envLookup arrElem env
deriveNumericExpr env (CVar (Ident iden _ _) _) = envLookup iden env
deriveNumericExpr env (CUnary CMinOp expr _) =
  let e = deriveNumericExpr env expr
  in -e
deriveNumericExpr env (CBinary CMulOp lhs rhs _) =
  let l = deriveNumericExpr env lhs
      r = deriveNumericExpr env rhs
  in l * r
deriveNumericExpr env (CBinary CDivOp lhs rhs _) =
  let l = deriveNumericExpr env lhs
      r = deriveNumericExpr env rhs
  in l `sDiv` r
deriveNumericExpr env (CBinary CRmdOp lhs rhs _) =
  let l = deriveNumericExpr env lhs
      r = deriveNumericExpr env rhs
  in l `sRem` r
deriveNumericExpr env (CBinary CAddOp lhs rhs _) = 
  let l = deriveNumericExpr env lhs
      r = deriveNumericExpr env rhs
  in l + r
deriveNumericExpr env (CBinary CSubOp lhs rhs _) = 
  let l = deriveNumericExpr env lhs
      r = deriveNumericExpr env rhs
  in l - r
deriveNumericExpr _ e = error $ "This numeric expression type is not implemented: "++(show e)

truthValue :: CConst -> Predicate
truthValue c = do
  let val = (unpackedConst c)::Int
  if val == 0
    then return sFalse
    else return sTrue

unpackedConst :: (Num a) => CConst -> a
unpackedConst (CIntConst (CInteger i _ _) _) = (fromIntegral i)
unpackedConst _ = error "This constant conversion not implemented."

sbvCompOp :: (OrdSymbolic b) => CBinaryOp -> (b -> b -> SBool)
sbvCompOp CLeqOp = (.<=)
sbvCompOp CGeqOp = (.>=)
sbvCompOp CLeOp = (.<)
sbvCompOp CGrOp = (.>)
sbvCompOp CEqOp = (.==)
sbvCompOp CNeqOp = (./=)
sbvCompOp op = error $ "Not a comparison operator: "++(show op)

sbvLogicOp :: CBinaryOp -> (SBool -> SBool -> SBool)
sbvLogicOp CLndOp = (.&&)
sbvLogicOp CLorOp = (.||)
sbvLogicOp op = error $ "Not a boolean operator: "++(show op)

sbvPtrOp :: (Num b) => CBinaryOp -> (b -> b -> b)
sbvPtrOp CAddOp = (+)
sbvPtrOp CSubOp = (-)
sbvPtrOp op = error $ "Not an arithmetic operator: "++(show op)

extractCIndexes :: CExpr -> [CExpr]
extractCIndexes e@(CIndex _ _ _) = [e]
extractCIndexes (CComma es _) = concat $ map extractCIndexes es
extractCIndexes (CCond e1 Nothing e2 _) = concat $ map extractCIndexes [e1,e2]
extractCIndexes (CCond e1 (Just e2) e3 _) = concat $ map extractCIndexes [e1,e2,e3]
extractCIndexes (CBinary _ l r _) = concat $ map extractCIndexes [l,r]
extractCIndexes (CCast _ e _) = extractCIndexes e
extractCIndexes (CUnary _ e _) = extractCIndexes e
extractCIndexes (CSizeofExpr e _) = extractCIndexes e
extractCIndexes (CSizeofType _ _) = []
extractCIndexes (CAlignofExpr e _) = extractCIndexes e
extractCIndexes (CAlignofType _ _) = []
extractCIndexes (CComplexReal e _) = extractCIndexes e
extractCIndexes (CComplexImag e _) = extractCIndexes e
extractCIndexes (CCall _ es _) = concat $ map extractCIndexes es
extractCIndexes (CMember _ _ _ _) = []
extractCIndexes (CVar _ _) = []
extractCIndexes (CConst _) = []
extractCIndexes (CCompoundLit _ _ _) = []
extractCIndexes (CStatExpr _ _) = []
extractCIndexes (CLabAddrExpr _ _) = []
extractCIndexes _ = error "No more expressions to explore."

extractVarName :: CExpr -> String
extractVarName (CIndex (CVar iden _) (CConst (CIntConst c _)) _) =
  let const' = getCInteger c
      idxStr = show const'
      idStr = identToString iden
      arrElem = idStr++"["++idxStr++"]"
  in arrElem
extractVarName _ = error "Expecting the CExpr to be a CIndex"

isSatRes :: (Conjunction, SatResult) -> Bool
isSatRes (_, (SatResult (Satisfiable _ _))) = True
isSatRes _ = False

isUnknownRes :: (Conjunction, SatResult) -> Bool
isUnknownRes (_, (SatResult (Unknown _ _))) = True
isUnknownRes (_, (SatResult (ProofError _ _ _))) = True
isUnknownRes _ = False

isUnsatRes :: (Conjunction, SatResult) -> Bool
isUnsatRes (_, (SatResult (Unsatisfiable _ _))) = True
isUnsatRes _ = False

isProven :: (Conjunction, ThmResult) -> Bool
isProven (_, (ThmResult (Unsatisfiable _ _))) = True
isProven _ = False

impliesTrue :: ThmResult -> Bool
impliesTrue (ThmResult (Unsatisfiable _ _)) = True
impliesTrue _ = False

hasCounterExample :: (Conjunction, ThmResult) -> Bool
hasCounterExample (_, (ThmResult (Satisfiable _ _))) = True
hasCounterExample _ = False

proofUnknown  :: (Conjunction, ThmResult) -> Bool
proofUnknown (_, (ThmResult (Unknown _ _))) = True
proofUnknown (_, (ThmResult (ProofError _ _ _))) = True
proofUnknown _ = False
\end{code}
