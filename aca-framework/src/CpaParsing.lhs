\begin{code}
module CpaParsing where

import qualified Data.Map.Strict as Map
import CscTypes
import Reading
import Data.List
import Data.List.Split
import Data.Char (isDigit)
import Data.Maybe

isNumeric :: String -> Bool
isNumeric ('.':_) = True
isNumeric ('-':_) = True
isNumeric (c:_) = isDigit c
isNumeric _ = False

-- we just need to check the first character;
-- we're not doing full-blown parsing
isCeeOp :: String -> Bool
isCeeOp ('+':_) = True
isCeeOp ('-':_) = True
isCeeOp ('*':_) = True
isCeeOp ('/':_) = True
isCeeOp ('%':_) = True
isCeeOp ('<':_) = True
isCeeOp ('>':_) = True
isCeeOp ('=':_) = True
isCeeOp ('!':_) = True
isCeeOp ('&':_) = True
isCeeOp ('|':_) = True
isCeeOp ('~':_) = True
isCeeOp ('^':_) = True
isCeeOp ('?':_) = True
isCeeOp (':':_) = True
isCeeOp _ = False

isSymVar :: String -> Bool
isSymVar s =
  ('#' `elem` s) &&
  (("aca_input_var_" `isInfixOf` s) ||
  ("initialize_reads::tmp_aca_input_arr_" `isInfixOf` s))

validSymbolicToken :: String -> Bool
validSymbolicToken t =
  (isNumeric t) || (isCeeOp t) || (isSymVar t)

isSymbolicExpr :: String -> Bool
isSymbolicExpr s =
  let ss = splitOn " " s
  in all validSymbolicToken ss

replacePound :: String -> String
replacePound s =
  if '#' `elem` s
    then
      let ss = splitOn "#" s
      in
        if null ss
          then error "list in replacePound is empty"
          else (head ss)++"["++(last ss)++"]"
    else s

replaceVarWithArr :: String -> String
replaceVarWithArr s =
  if "_var_" `isInfixOf` s
    then
      let ss = splitOn "_var_" s
      in
        if null ss
          then error "list in replaceVarWithArr is empty"
          else (head ss)++"_arr_"++(last ss)
    else s

removeInstrumentationPrefix :: String -> String
removeInstrumentationPrefix s =
  if "((_Bool)initialize_reads::tmp_" `isInfixOf` s
    then
      let s' = init $ drop 30 s
      in s'
    else
      if "initialize_reads::tmp_" `isInfixOf` s
        then drop 22 s
        else s

makeValidArray :: String -> String
makeValidArray s =
  let ss = splitOn " " s
      ss' = map removeInstrumentationPrefix ss
      ss'' = map replacePound ss'
      ss''' = map replaceVarWithArr ss''
  in unwords ss'''

grabIdStr :: String -> String
grabIdStr s =
  let ss = splitOn "[" s
  in 
    if null ss
      then error "empty list in grabIdStr"
      else head ss

grabIdxStr :: String -> String
grabIdxStr s =
  let ss = splitOn "]" s
  in
    if null ss
      then error "empty list in grabIdxStr"
      else head ss

grabIdTypeIndices :: String -> IO [(Int, TypeString, Int)]
grabIdTypeIndices s = do
  let tokens = splitOn " " s
      names = filter ("aca_input_arr" `isInfixOf`) tokens
      triples = map grabTriple names      
  return triples

findMaxIdx :: [SymVar] -> (Int, Int)
findMaxIdx [] = error "list in findMaxIdx is empty"
findMaxIdx ss =
  let (SymVar identifier _ _) = head ss
      indices = map (\(SymVar _ _ idx)->idx) ss
      maxIdx = maximum indices
  in (identifier,(maxIdx+1))

grabIdType :: [SymVar] -> (Int, TypeString)
grabIdType [] = error "list in grabIdType is empty"
grabIdType ss =
  let (SymVar identifier ty _) = head ss
  in (identifier,ty)

-- this function gives the symbolic names of each unique
-- input ID ascending array indices grounded at 0
groundInputIds :: [SymExpr] -> [SymExpr]
groundInputIds es =
  let symVars = nub $ concat $ map extractSymVars es
      groundedMap = recoverGrounding symVars
      es' = ground groundedMap es
  in es'

makeSymExpr :: String -> SymExpr
makeSymExpr s =
  let tokens = splitOn " " s
      symExpr = map makeExprTokens tokens
  in symExpr

makeExprTokens :: String -> ExprToken
makeExprTokens s =
  if isSymVar s
    then
      let s' = makeValidArray s
          (identifier,tyStr,index) = grabTriple s'
      in VarToken (SymVar identifier tyStr index)
    else Token s
  
type SymExpr = [ExprToken]
data ExprToken = Token String | VarToken SymVar deriving (Eq,Show)
type Identifier = Int
type Index = Int
data SymVar = SymVar Identifier TypeString Index deriving (Eq,Show)
instance Ord SymVar where
  compare (SymVar i1 _ _) (SymVar i2 _ _) = compare i1 i2

grabTriple :: String -> (Int, TypeString, Int)
grabTriple s = 
  let tokens = splitOn "_" s
      tyString = tokens !! 3
      idString = grabIdStr (last tokens)
      identity = read idString::Int
      indexStr = grabIdxStr (last $ splitOn "[" s)
      index = read indexStr::Int
  in (identity,tyString,index)

extractSymVars :: SymExpr -> [SymVar]
extractSymVars e =
  let ss = filter isSymVarToken e
      ss' = map unpackSymVar ss
  in ss'

unpackSymVar :: ExprToken -> SymVar
unpackSymVar (VarToken s) = s
unpackSymVar _ = error "should not hit this case in unpackSymVar"

isSymVarToken :: ExprToken -> Bool
isSymVarToken (VarToken _) = True
isSymVarToken _ = False

recoverGrounding :: [SymVar] -> Map.Map SymVar SymVar
recoverGrounding vs =
  let idGroups = groupBy (\(SymVar i1 _ _) (SymVar i2 _ _)->i1==i2) vs
      idGroups' = map nub idGroups
      idGroups'' = map (sortBy (\(SymVar _ _ idx1) (SymVar _ _ idx2)-> compare idx1 idx2)) idGroups'
      idGroups''' = concat $ map makeAssocPairs idGroups''
  in Map.fromList idGroups'''

makeAssocPairs :: [SymVar] -> [(SymVar,SymVar)]
makeAssocPairs [] = error "empty list in makeAssocPairs"
makeAssocPairs vs =
  let (SymVar identifier typeStr _) = head vs
      vsIds = zip [0..] vs
  in map (\(i,symVar)->(symVar,(SymVar identifier typeStr i))) vsIds

ground :: Map.Map SymVar SymVar -> [SymExpr] -> [SymExpr]
ground groundMap es = map (groundExpr groundMap) es

groundExpr :: Map.Map SymVar SymVar -> SymExpr -> SymExpr
groundExpr groundMap es = map (groundExprToken groundMap) es

groundExprToken :: Map.Map SymVar SymVar -> ExprToken -> ExprToken
groundExprToken _ e@(Token _) = e
groundExprToken groundMap (VarToken s) =
  let maybeS = Map.lookup s groundMap
  in
    if isJust maybeS
      then
        let (Just groundedExpr) = maybeS
        in VarToken groundedExpr
      else error $ "this symbolic var isn't in the grounded map: "++(show s)

-- we have the C expressions, but still need to update
-- the count map and the type map
extractCpaSubspace :: [String] -> IO (Maybe Subspace)
extractCpaSubspace ss = do
  let symExprs = map makeSymExpr ss
      symExprs' = groundInputIds symExprs
  let symVars = nub $ concat $ map extractSymVars symExprs'
      symVarsById = groupBy (\(SymVar i1 _ _) (SymVar i2 _ _)->i1==i2) symVars
      idMaxIdxPairs = map findMaxIdx symVarsById
      idTypePairs = map grabIdType symVarsById
      countMap = Map.fromList idMaxIdxPairs
      typeMap = Map.fromList idTypePairs
      ss' = map symExprToString symExprs'
  cs <- mapM parseToExpr ss'
  let cs' = map Conjunct cs
      as = [] -- no assumptions right now
      subspace = Subspace cs' as countMap typeMap
  return (Just subspace)

symExprToString :: SymExpr -> String
symExprToString ts =
  let tokenStrs = map exprTokenToString ts
  in unwords tokenStrs

exprTokenToString :: ExprToken -> String
exprTokenToString (Token s) = s
exprTokenToString (VarToken (SymVar identifier typeStr idx)) =
  "aca_input_arr_"++typeStr++"_"++(show identifier)++"["++(show idx)++"]"
\end{code}
