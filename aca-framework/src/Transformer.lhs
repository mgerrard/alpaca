\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Transformer where
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List (isInfixOf, isPrefixOf, partition, intercalate)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Control.Exception.Base (assert)

import Language.C
import Language.C.System.GCC
import Language.C.Data.Ident

import Transformations
import CscTypes

{-

The AST transformations fall into one of three stages.
We run the first two passes at the start of ACA and then
run the update pass for each iteration of ACA.

First Pass:
 - replace local declarations and assigns having a
   call to __VERIFIER_nondet_*() on the RHS with
   an if-else block and a tail decl/assign of an
   instrumentation variable
 - track count and type of input variables
 - if the target function is different from __VERIFIER_error,
   just prepend the statement:
     if (1) { __VERIFIER_error(); }
   to each call of the target function

Second Pass:
 - Add global initialization of instrumentation variables
   before all function calls
 - ensure existence of extern void __VERIFIER_assume(int);
 - insert initialize_reads() function
 - insert block_subspace() function
 - insert calls to the above functions at the top of main()

Update:
 - replace RHS of set of global declarations
 - replace body of block_subspace() with new CSC upper bound

-}

type Transformer a = State TransState a

data Stage = FirstPass | SecondPass | Update | RestrictToGap deriving (Eq, Show)
data TransState = TransState
  { count :: Int,
    hoistedVarCount :: Int,
    stage :: Stage,
    inputReads :: [InputRead],
    transCsc :: Maybe Csc,
    transPartition :: Maybe CscPartition,
    targetFunction :: String,
    dseUsed :: DseTool,
    maybeCud :: Maybe String
  }
  
incrCount :: Transformer ()
incrCount = modify (\st -> st {count = 1 + (count st)})

incrHoistedVarCount :: Transformer ()
incrHoistedVarCount = modify (\st -> st {hoistedVarCount = 1 + (hoistedVarCount st)})

updateReadType :: CDeclSpec -> Transformer ()
updateReadType spec =
  modify (\st -> st {inputReads = ((InputRead { inputId = (count st), inputType = spec, inputCount = 0}) : (inputReads st))})

initializeReadsBody :: Bool -> [InputRead] -> CStat
initializeReadsBody isCivl xs =
  let rs = reverse xs
      stmts = map (\y -> forStatement isCivl (inputId y) (show $ pretty (inputType y))) rs
      blockItems = map CBlockStmt stmts
  in CCompound [] blockItems undefNode

initializeReadsFunc :: Bool -> [InputRead] -> CFunDef
initializeReadsFunc isCivl rs =
  let iden = Ident "initialize_reads()" 0 undefNode
      declr = CDeclr (Just iden) [] Nothing [] undefNode
      b = initializeReadsBody isCivl rs
  in CFunDef [voidSp] declr [] b undefNode

transformAst :: CTranslUnit -> Transformer CTranslUnit
transformAst (CTranslUnit es n) = do
  es' <- mapM transformExtDecl es
  st <- get
  if (stage st) == SecondPass
    then do
      let readsMap = inputReads st
      let newDecls = map makeGlobalQuadruples readsMap
      let isCivl = (dseUsed st)==CivlSymExec
      let initFunc = initializeReadsFunc isCivl readsMap
      let instrDefs = (join newDecls) ++ [(CFDefExt initFunc), (CFDefExt blockSubspaceFunc)]
      let newDefs = placeInstrumentationAtBeginning instrDefs es'
      let newDefsWithAssume = ensureAssume newDefs
      return $ CTranslUnit (newDefsWithAssume) n
    else do
      return $ CTranslUnit es' n

placeInstrumentationAtBeginning :: [CExtDecl] -> [CExtDecl] -> [CExtDecl]
placeInstrumentationAtBeginning instrDefs origDefs = instrDefs ++ origDefs

ensureAssume :: [CExtDecl] -> [CExtDecl]
ensureAssume xs =
  if (any isExternAssume xs)
    then
      {- We want to place the assume declaration before our instrumentation -}
      let xs' = filter (not . isExternAssume) xs
      in [assumeDeclaration] ++ xs' 
    else [assumeDeclaration] ++ xs

isExternAssume :: CExtDecl -> Bool
isExternAssume (CDeclExt (CDecl _ [(Just declr, _, _)] _)) = functionNameIs "__VERIFIER_assume" declr
isExternAssume _ = False

assumeDeclaration :: CExtDecl
assumeDeclaration = CDeclExt (CDecl
                              [externSpecifier, voidSp]
                              [(Just (makeDeclarator "__VERIFIER_assume(int)"), Nothing, Nothing)]
                              undefNode)

externSpecifier :: CDeclSpec
externSpecifier = CStorageSpec (CExtern undefNode)

moveInstrumentationToEnd :: [CExtDecl] -> [CExtDecl] -> [CExtDecl]
moveInstrumentationToEnd instrDefs origDefs =
  let defsPartition = partition isMain origDefs
      mainDef = fst defsPartition
      nonMainDefs = snd defsPartition
  in nonMainDefs ++ instrDefs ++ mainDef

transformExtDecl :: CExtDecl -> Transformer CExtDecl
transformExtDecl (CDeclExt d) = do
  st <- get
  let theStage = stage st
  if (theStage == Update) || (theStage == RestrictToGap)
    then do
      let (Just c) = transCsc st
      d' <- transformAcaDecls d c
      return $ CDeclExt d'
    else do
      d' <- transformDecl d
      return $ CDeclExt d'
transformExtDecl (CFDefExt d) = do
  d' <- transformFDef d
  return $ CFDefExt d'

transformExtDecl (CAsmExt d n) = do
  d' <- transformAsm d
  return $ CAsmExt d' n

transformAcaDecls :: CDecl -> Csc -> Transformer CDecl
transformAcaDecls decl csc
  | isAcaNumGlobal decl = updateNumGlobal decl csc
  | isAcaArrGlobal decl = updateArrGlobal decl csc
  | otherwise           = transformDecl decl

getAcaGlobalName :: CDecl -> String
getAcaGlobalName (CDecl _ [((Just (CDeclr (Just iden) _ _ _ _)),_,_)] _) =
  let (Ident n _ _) = iden
  in n
getAcaGlobalName d = error $ "I can't get the ACA global name from: "++(show d)  

getAcaGlobalTypespec :: CDecl -> CDeclSpec
getAcaGlobalTypespec (CDecl specs _ _) = assert (not (null specs)) $ head specs
getAcaGlobalTypespec d = error $ "I can't get the ACA global typespec from: "++(show d)

isAcaNumGlobal :: CDecl -> Bool
isAcaNumGlobal (CDecl _ [((Just (CDeclr (Just iden) _ _ _ _)),_,_)] _) =
  let (Ident n _ _) = iden
  in "aca_input_num_" `isPrefixOf` n
isAcaNumGlobal _ = False

isAcaArrGlobal :: CDecl -> Bool
isAcaArrGlobal (CDecl _ [((Just (CDeclr (Just iden) _ _ _ _)),_,_)] _) =
  let (Ident n _ _) = iden
  in "aca_input_arr_" `isPrefixOf` n
isAcaArrGlobal _ = False

updateNumGlobal :: CDecl -> Csc -> Transformer CDecl
updateNumGlobal decl (Csc _ inputMap _ _ _) = do
  if null inputMap
    then do return decl
    else do
      let n = getAcaGlobalName decl
          idStr = last $ splitOn "_" n
          i = (read idStr) :: Int
          inMap = Map.lookup i inputMap
      if isJust inMap
        then do
          let (Just cnt) = inMap
          return (declareVarInit intType n (show cnt))
        else do return decl

updateArrGlobal :: CDecl -> Csc -> Transformer CDecl
updateArrGlobal decl (Csc _ inputMap _ _ _) =
  if null inputMap
    then do return decl
    else do
      let n = getAcaGlobalName decl
          typeSpec = getAcaGlobalTypespec decl
          idSuff = last $ splitOn "_" n
          idStr = splitOn "[" idSuff
          idStr' = assert (not (null idStr)) $ head idStr
          i = (read idStr') :: Int
          inMap      = Map.lookup i inputMap
      if isJust inMap
        then do
          let (Just cnt) = inMap
              acaArr = "aca_input_arr_"++(typeInfix typeSpec)++(show i)++"["++(show cnt)++"]"
          return (declareVar typeSpec acaArr)
        else do return decl

transformDecl :: CDecl -> Transformer CDecl
transformDecl (CDecl a b c) = return $ CDecl a b c
transformDecl (CStaticAssert a b c) = return $ CStaticAssert a b c

initIsRead :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] -> Bool
initIsRead [] = assert False False
initIsRead i =
  let (_, (Just ini), _) = head i in
    hasFreshReadCall ini

hasFreshReadCall :: CInit -> Bool
hasFreshReadCall (CInitExpr e _) = exprIsReadCall e
hasFreshReadCall (CInitList ls _) = any (hasFreshReadCall . snd) ls

exprIsReadCall :: CExpr -> Bool
exprIsReadCall (CCall (CVar f _) _ _) =
  let functionName = identToString f in
    if "VERIFIER_nondet" `isInfixOf` functionName
      then True
      else False
exprIsReadCall _ = False

isAssertCall :: CExpr -> Bool
isAssertCall (CCall (CVar f _) _ _) =
  let functionName = identToString f in
    if "assert" `isInfixOf` functionName
      then True
      else False
isAssertCall _ = False

isPrintCall :: CExpr -> Bool
isPrintCall (CCall (CVar f _) _ _) =
  let functionName = identToString f in
    if (("print" `isInfixOf` functionName) || ("puts" `isInfixOf` functionName))
      then True
      else False
isPrintCall _ = False

isTargetFunction :: CExpr -> String -> Bool
isTargetFunction _ "__VERIFIER_error" = False
isTargetFunction (CCall (CVar f _) _ _) target =
  let functionName = identToString f in
    if (functionName == target)
      then True 
      else False
isTargetFunction _ _ = False

type DeclListElem = (Maybe CDeclr, Maybe CInit, Maybe CExpr)

transformStdDecl :: DeclListElem -> Transformer DeclListElem
transformStdDecl e = do
  return e

makeCCallBlockItem :: String -> CBlockItem
makeCCallBlockItem funcName =
  CBlockStmt (CExpr (Just (CCall (makeVar funcName) [] undefNode)) undefNode)

transformFDef :: CFunDef -> Transformer CFunDef
transformFDef (CFunDef p1 declr p3 b p5) = do
  body' <- transformStat b
  st <- get
  let body'' = transformFunBodies (stage st) (maybeCud st) declr body' (transCsc st) (transPartition st) (targetFunction st)
  return $ CFunDef p1 declr p3 body'' p5

prependAssertion :: CStat -> CStat
prependAssertion s = CCompound [] [CBlockStmt trueWrappedError, CBlockStmt s] undefNode

transformFunBodies :: Stage -> Maybe String -> CDeclr -> CStat -> Maybe Csc -> Maybe CscPartition -> String -> CStat
transformFunBodies theStage mCud declr origBody csc p targetFunc
  | theStage == FirstPass =
      if (targetFunc == "__VERIFIER_error")
         then origBody
         else if (functionNameIs targetFunc declr)
                 then prependAssertion origBody
                 else origBody
  | theStage == SecondPass =
      if (functionNameIs "main" declr)
        then
          let iCall = makeCCallBlockItem "initialize_reads"
              bCall = makeCCallBlockItem "block_subspace"
          in
            if (isJust mCud)
              then
                let (Just cud) = mCud
                    cudAssume = blockCudSubspace cud
                    newBody = CCompound [] [iCall, cudAssume, bCall, CBlockStmt origBody] undefNode
                in newBody
              else
                let newBody = CCompound [] [iCall, bCall, CBlockStmt origBody] undefNode
                in newBody
        else origBody
  | theStage == Update =
      if (functionNameIs "block_subspace()" declr)
        then
          let (Just (Csc partitions _ _ spurious _)) = csc
          in
            if ((null partitions) && (null spurious))
              then makeReturnBody
              else makeBlockingBody partitions spurious
        else origBody
  | theStage == RestrictToGap =
      if (functionNameIs "block_subspace()" declr)
        then
          -- precondition: some gap exists between upper and lower
          let (Just (CscPartition up lower _)) = p
          in restrictToGap up lower
        else origBody
  | otherwise = error $ "I don't know how to transform this function body: "++(show declr)

restrictToGap :: UpperBound -> LowerBound -> CStat             
restrictToGap ub lb =
  let upperAssumptions = assumeUpper ub
      lowerAssumptions = assumeNotLower lb
  in CCompound [] (upperAssumptions ++ lowerAssumptions) undefNode

assumeUpper :: UpperBound -> [CBlockItem]
assumeUpper ub =
  let upperA = assumeSubspace (upper ub)
      upperNs = map blockSubspace (upperNegations ub)
  in [upperA] ++ upperNs

assumeNotLower :: LowerBound -> [CBlockItem]
assumeNotLower lb = map blockSubspace lb

makeBlockingBody :: [CscPartition] -> [Conjunction] -> CStat
makeBlockingBody partitions validPrefixes =
  let upperBounds = map (upper . upperBound) partitions
      assumeStmts = map blockSubspace (upperBounds ++ validPrefixes)
  in CCompound [] assumeStmts undefNode

makeReturnBody :: CStat
makeReturnBody = CCompound [] [returnBlockItem] undefNode

returnBlockItem :: CBlockItem
returnBlockItem = CBlockStmt (CReturn Nothing undefNode)

makeAssumeBody :: [CscPartition] -> CStat
makeAssumeBody partitions =
  let as = concat $ map assumptions partitions
      assumeStmts = map assumeSubspace as
  in CCompound [] assumeStmts undefNode

blockCudSubspace :: String -> CBlockItem
blockCudSubspace s =
  let assump = "__VERIFIER_assume"++s
      stmt = (CExpr (Just (makeVar assump))) undefNode
  in CBlockStmt stmt

blockSubspace :: Conjunction -> CBlockItem
blockSubspace conj =
  let prefix         = "__VERIFIER_assume(!("
      ceeConjunction = map (\(Conjunct c)-> "("++(show $ pretty c)++")") conj
      expr           = intercalate " && " ceeConjunction
      suffix         = "))"
      stmt           = (CExpr (Just (makeVar $ prefix <> expr <> suffix))) undefNode
  in CBlockStmt stmt

assumeSubspace :: Conjunction -> CBlockItem
assumeSubspace conj =
  let prefix         = "__VERIFIER_assume("
      ceeConjunction = map (\(Conjunct c)-> "("++(show $ pretty c)++")") conj
      expr           = intercalate " && " ceeConjunction
      suffix         = ")"
      stmt           = (CExpr (Just (makeVar $ prefix <> expr <> suffix))) undefNode
  in CBlockStmt stmt

transformStat :: CStat -> Transformer CStat
transformStat (CLabel p1 s p3 p4) = do
  s' <- transformStat s
  return $ CLabel p1 s' p3 p4
transformStat (CCase p1 s p3) = do
  s' <- transformStat s
  return $ CCase p1 s' p3
transformStat (CCases p1 p2 s p4) = do
  s' <- transformStat s
  return $ CCases p1 p2 s' p4
transformStat (CDefault s p2) = do
  s' <- transformStat s
  return $ CDefault s' p2
transformStat (CExpr (Just e@(CCall _ args _)) p2) = do
  if isAssertCall e
    then do
      let condition = head args --assert has one expression argument
      let errorStatement = (CExpr (Just verifierErrorCall) undefNode)
      return (CIf condition emptyStatement (Just errorStatement) undefNode)
    else if isPrintCall e --we ignore calls to printf, sprintf, puts, etc.
      then do
        return $ CExpr Nothing undefNode
      else do
        e' <- transformExpr e
        return $ CExpr (Just e') p2
transformStat (CExpr expr p2) = do
  if isJust expr
    then do
      let (Just e) = expr
      e' <- transformExpr e
      return $ CExpr (Just e') p2
    else do return $ CExpr Nothing p2
transformStat (CCompound p1 b p3) = do
  b' <- mapM transformBlockItem b
  return $ CCompound p1 (join b') p3
transformStat s@(CIf p1 _ _ _) = do
  if exprIsReadCall p1
    then hoistReadAndTransformIf s
    else transformIf s
transformStat (CSwitch p1 s p3) = do
  s' <- transformStat s
  return $ CSwitch p1 s' p3
transformStat s@(CWhile p1 _ _ _) = do
  if exprIsReadCall p1
    then hoistReadAndTransformWhile s
    else transformWhile s
transformStat (CFor p1 p2 p3 s p5) = do
  s' <- transformStat s
  return $ CFor p1 p2 p3 s' p5
transformStat (CGoto p1 p2) = return $ CGoto p1 p2
transformStat (CGotoPtr p1 p2) = return $ CGotoPtr p1 p2
transformStat (CCont p1) = return $ CCont p1
transformStat (CBreak p1) = return $ CBreak p1
transformStat (CReturn expr p2) = do
  if isJust expr
    then do
      let (Just e) = expr
      e' <- transformExpr e
      return $ CReturn (Just e') p2
    else do return $ CReturn Nothing p2              
transformStat (CAsm p1 p2) = return $ CAsm p1 p2

trueWrappedError :: CStat
trueWrappedError =
  let errorStatement = (CExpr (Just verifierErrorCall) undefNode)
      trueWrap = CIf (makeExpr "1") errorStatement Nothing undefNode
  in trueWrap

transformWhile :: CStat -> Transformer CStat
transformWhile (CWhile p1 s1 p3 p4) = do
  let p1' = transformCondition p1
  s1' <- transformStat s1
  return $ CWhile p1' s1' p3 p4
transformWhile s = error $ "This statement should be a 'while', but it's not: "++(show s)  

hoistReadAndTransformWhile :: CStat -> Transformer CStat
hoistReadAndTransformWhile (CWhile p1 s1 p3 p4) = do
  st <- get
  let c = hoistedVarCount st
  incrHoistedVarCount
  let hoistedVarName = hoistedVarPrefix ++ (show c)
  let hoistedVar = makeVar hoistedVarName
  let spec = grabReadCallTypeSpec p1
  let func = (grabFuncCallId p1)++"()"
  let hoistedDecl = declareVarInit spec hoistedVarName func
  hoistedDecl' <- transformDeclInstr hoistedDecl
  let hoistedVarUpdate = CAssign CAssignOp hoistedVar p1 undefNode
  hoistedVarUpdate' <- transformAssign hoistedVarUpdate
  let statementBlock = CCompound [] ([(CBlockStmt s1)]++hoistedVarUpdate') undefNode
  whileStmt <- transformWhile (CWhile hoistedVar statementBlock p3 p4)
  let whileBlock = CBlockStmt whileStmt
  return $ CCompound [] (hoistedDecl'++[whileBlock]) undefNode
hoistReadAndTransformWhile s = error $ "This statement should be a 'while', but it's not: "++(show s)  

transformIf :: CStat -> Transformer CStat
transformIf (CIf p1 s1 s2 p4) = do
  let p1' = transformCondition p1
  s1' <- transformStat s1
  if isJust s2
    then do
      let (Just s2Body) = s2
      s2Body' <- transformStat s2Body
      return $ CIf p1' s1' (Just s2Body') p4
    else do return $ CIf p1' s1' Nothing p4
transformIf s = error $ "This statement should be an 'if', but it's not: "++(show s)            

transformCondition :: CExpr -> CExpr
transformCondition v@(CVar _ _) = notEqualsZero v
transformCondition (CUnary unaryOp v a) = (notEqualsZero (CUnary unaryOp v a))
transformCondition c = c

hoistReadAndTransformIf :: CStat -> Transformer CStat
hoistReadAndTransformIf (CIf p1 s1 s2 p4) = do
  st <- get
  let c = hoistedVarCount st
  incrHoistedVarCount
  let hoistedVarName = hoistedVarPrefix ++ (show c)
  let hoistedVar = makeVar hoistedVarName
  let spec = grabReadCallTypeSpec p1
  let funcName = grabFuncCallId p1
  let hoistedDecl = declareVarInit spec hoistedVarName (funcName++"()")
  hoistedDecl' <- transformDeclInstr hoistedDecl
  ifStmt <- transformIf (CIf hoistedVar s1 s2 p4)
  let ifBlock = CBlockStmt ifStmt
  return $ CCompound [] (hoistedDecl'++[ifBlock]) undefNode
hoistReadAndTransformIf s = error $ "This statement should be an 'if', but it's not: "++(show s)            

hoistedVarPrefix :: String
hoistedVarPrefix = "aca_hoisted_var_"

isAssign :: CStat -> Bool
isAssign (CExpr (Just (CAssign _ _ _ _)) _) = True
isAssign (CExpr (Just (CCompoundLit _ _ _)) _) = True
isAssign _ = False

rhsHasRead :: CStat -> Bool
rhsHasRead (CExpr (Just (CAssign _ _ rVal _)) _) = "VERIFIER_nondet" `isInfixOf` (show $ pretty rVal)
rhsHasRead _ = False

transformBlockItem :: CBlockItem -> Transformer [CBlockItem]
transformBlockItem (CBlockStmt s) = do
  if ((isAssign s) && (rhsHasRead s))
    then do
      st <- get
      if (stage st) == FirstPass
        then do
          let (CExpr (Just a) _) = s
          transformAssign a
        else do
          return $ [CBlockStmt s]
    else do
      s' <- transformStat s
      return $ [CBlockStmt s']
transformBlockItem (CBlockDecl d@(CDecl a b c)) = do
  let hasOneInit = any (\(_, p2, _) -> isJust p2) b
             && (length b == 1)
  if (hasOneInit && (initIsRead b))
    then do
      st <- get
      if (stage st) == FirstPass
        then do
          transformDeclInstr d
        else do
          return $ [CBlockDecl $ CDecl a b c]         
    else do
      return $ [CBlockDecl $ CDecl a b c]
transformBlockItem (CNestedFunDef d) = return $ [CNestedFunDef d]
transformBlockItem s = error $ "I don't know how to handle this block statement: "++(show s)

transformExpr :: CExpr -> Transformer CExpr
transformExpr e = do
  return e

transformAsm :: CStrLit -> Transformer CStrLit
transformAsm d = return d

firstPassTransform :: CTranslUnit -> String -> CTranslUnit
firstPassTransform ast targetFunc = evalState (transformAst ast) (TransState 0 0 FirstPass [] Nothing Nothing targetFunc CivlSymExec Nothing)

countAfterFirstPass :: CTranslUnit -> Int
countAfterFirstPass ast = count $ execState (transformAst ast) (TransState 0 0 FirstPass [] Nothing Nothing "" CivlSymExec Nothing)

initialTransform :: CTranslUnit -> String -> IO CTranslUnit
initialTransform ast funcName = wrapMainAroundFunc ast funcName

wrapMainAroundFunc :: CTranslUnit -> String -> IO CTranslUnit
wrapMainAroundFunc ast funcName = makeModularAst ast funcName

twoPassTransform :: CTranslUnit -> String -> DseTool -> Maybe String -> CTranslUnit
twoPassTransform ast targetFunc dTool mCud =
  let ast'    = firstPassTransform ast targetFunc
      fpState = execState (transformAst ast) (TransState 0 0 FirstPass [] Nothing Nothing targetFunc dTool Nothing)
      c       = count fpState
      hc      = hoistedVarCount fpState
      rTypes  = inputReads fpState
  in
    evalState (transformAst ast') (TransState c hc SecondPass rTypes Nothing Nothing "" dTool mCud)

updateTransform :: CTranslUnit -> Csc -> CTranslUnit
updateTransform ast csc = evalState (transformAst ast) (TransState 0 0 Update [] (Just csc) Nothing "" CivlSymExec Nothing)

restrictToGapTransform :: CTranslUnit -> Csc -> CscPartition -> CTranslUnit
restrictToGapTransform ast csc p = evalState (transformAst ast) (TransState 0 0 RestrictToGap [] (Just csc) (Just p) "" CivlSymExec Nothing)

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

makeTypedRead :: String -> CExpr
makeTypedRead ty =
  let ty'   = typeTag ty
      fId   = Ident ("__VERIFIER_nondet_" ++ ty' ++ "()") 0 undefNode
      fVar  = CVar fId undefNode
      fCall = CCall fVar [] undefNode
  in fCall

intRead :: CExpr
intRead = makeTypedRead "int"

transformDeclInstr :: CDecl -> Transformer [CBlockItem]
transformDeclInstr decl = do
  st <- get
  let c        = count st
  let origVar  = extractDeclVarStr decl
  let typeStr  = extractInitTypeStr decl
  let typeSpec = extractInitType decl
  let tyInfix  = typeInfix typeSpec
  let newVar   = "aca_input_var_"++tyInfix++(show c)
  let ifElse   = makeInstrIfElseExpr c newVar tyInfix typeStr
  -- make a new string out of ((inferTypeStr :: CDeclSpec -> String)++origVar) newVar
  let newDecl = varInit ((transformInfer typeSpec)++" "++origVar) newVar
--  let newDecl = declareVarInit typeSpec origVar newVar
  updateReadType typeSpec
  incrCount  
  return [(CBlockStmt ifElse),(CBlockDecl newDecl)]

transformInfer :: CDeclSpec -> String
transformInfer s =
  let str = inferTypeStr s
  in
    if str=="pointer"
      then "void *"
      else str

transformAssign :: CExpr -> Transformer [CBlockItem]
transformAssign expr = do
  st <- get
  let c         = count st
  let lhs       = grabAssignLhs expr
  let fCallStr  = grabFuncCallId expr
  let typeStr   = grabTypeStr fCallStr
  let typeSpec  = makeTypeSpec typeStr
  let tyInfix  = typeInfix typeSpec
  let newVar    = "aca_input_var_"++tyInfix++(show c)
  let ifElse    = makeInstrIfElseExpr c newVar tyInfix typeStr
  let newAssign = CAssign CAssignOp lhs (makeVar newVar) undefNode
  updateReadType typeSpec
  incrCount
  return [(CBlockStmt ifElse),(CBlockStmt (CExpr (Just newAssign) undefNode))]
\end{code}
