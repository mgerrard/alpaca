\begin{code}
module Transformations where

import Data.Maybe
import Data.List (isInfixOf, find, intersect, nub)
import Data.List.Split (splitOn)
import Data.String.Utils (startswith)
import Control.Exception.Base (assert)
import qualified Data.Map.Strict as Map

import Language.C
import Language.C.Data.Ident

data InputRead = InputRead
  { inputId :: Int,
    inputType :: CDeclSpec,
    inputCount :: Int
  } deriving (Show)
  
extractDeclVarStr :: CDecl -> String
extractDeclVarStr decl =
  let (CDecl _ exprs _) = decl
      singleExpr = assert (not (null exprs)) $ head exprs
      (Just declr, _, _) = singleExpr
      (CDeclr (Just iden) _ _ _ _) = declr
      (Ident n _ _) = iden
   in n

extractInitTypeStr :: CDecl -> String
extractInitTypeStr decl =
  let (CDecl declSpec _ _) = decl
      singleSpec = assert (not (null declSpec)) $ head declSpec
      stringSpec = (show . pretty) singleSpec
  in stringSpec

extractInitType :: CDecl -> CDeclSpec
extractInitType decl =
  let (CDecl declSpec _ _) = decl
      singleSpec = assert (not (null declSpec)) $ head declSpec
  in singleSpec

extractInitTypeSpec :: CDecl -> CTypeSpec
extractInitTypeSpec decl =
  let (CDecl declSpec _ _) = decl
      singleSpec = assert (not (null declSpec)) $ head declSpec
      (CTypeSpec spec) = singleSpec
  in spec

extractInitTypeSpecM :: CDecl -> IO CTypeSpec
extractInitTypeSpecM decl = do
  putStrLn $ show decl
  let (CDecl declSpec _ _) = decl
      singleSpec = assert (not (null declSpec)) $ head declSpec
      (CTypeSpec spec) = singleSpec
  return spec

makeVar :: String -> CExpr
makeVar s =
  let iden = makeIdent s
  in CVar iden undefNode

makeExpr :: String -> CExpr
makeExpr s =
  let iden = makeIdent s
  in CVar iden undefNode

makeIdent :: String -> Ident
makeIdent s = Ident s 0 undefNode

addPossiblePointer :: String -> String
addPossiblePointer s =
  if "_pointer_" `isInfixOf` s
    then "* "++s
    else s

declareVar :: CDeclSpec -> String -> CDecl
declareVar ty s =
  let s' = addPossiblePointer s
      declr = makeDeclarator s'
  in CDecl [ty] [(Just declr, Nothing, Nothing)] undefNode

declareVarInit :: CDeclSpec -> String -> String -> CDecl
declareVarInit ty lhs rhs =
  let declr = makeDeclarator lhs
      vInit = makeDeclaratorRhs rhs
  in CDecl [ty] [(Just declr, Just vInit, Nothing)] undefNode

varInit :: String -> String -> CDecl
varInit lhs rhs =
  let declr = makeDeclarator lhs
      vInit = makeDeclaratorRhs rhs
  in CDecl [] [(Just declr, Just vInit, Nothing)] undefNode

makeDeclaratorRhs :: String -> CInit
makeDeclaratorRhs rhs = CInitExpr (makeVar rhs) undefNode

makeDeclarator :: String -> CDeclr
makeDeclarator s =
  let iden = makeIdent s
  in CDeclr (Just iden) [] Nothing [] undefNode

assignExpr :: String -> String -> CExpr
assignExpr lhs rhs =
  CAssign CAssignOp (makeVar lhs) (makeVar rhs) undefNode

makeLtExpr :: String -> String -> CExpr
makeLtExpr lOperand rOperand =
  CBinary CLeOp (makeVar lOperand) (makeVar rOperand) undefNode

makeIfElse :: CExpr -> CExpr -> CExpr -> CStat
makeIfElse cond ifExpr elseExpr =
  let ifBody = CExpr (Just ifExpr) undefNode
      elseBody = CExpr (Just elseExpr) undefNode
  in CIf cond ifBody (Just elseBody) undefNode

acaVarPrefix :: String
acaVarPrefix = "aca_input_var_"

acaNumPrefix :: String
acaNumPrefix = "aca_input_num_"

acaCounterPrefix :: String
acaCounterPrefix = "aca_input_counter_"

acaArrPrefix :: String
acaArrPrefix = "aca_input_arr_"

makeInstrCond :: Int -> CExpr
makeInstrCond count =
  let iden  = show count
      lOp = acaCounterPrefix ++ iden
      rOp = acaNumPrefix ++ iden
  in makeLtExpr lOp rOp

makeInstrElseExpr :: String -> String -> CExpr
makeInstrElseExpr varName ty =
  let ty'      = typeTag ty
      readCall = makeVar $ "__VERIFIER_nondet_" ++ ty' ++"()"
  in
  CAssign CAssignOp (makeVar varName) readCall undefNode

makeInstrIfExpr :: String -> String -> Int -> CExpr
makeInstrIfExpr varName tyInfix count =
  let array = makeVar $ acaArrPrefix ++ tyInfix ++ (show count)
      index = makeVar $ acaCounterPrefix ++ (show count) ++ "++"
  in
  CAssign CAssignOp (makeVar varName) (CIndex array index undefNode) undefNode

makeInstrIfElseExpr :: Int -> String -> String -> String -> CStat
makeInstrIfElseExpr count varName tyInfix typeStr =
  let cond = makeInstrCond count
      ifExpr = makeInstrIfExpr varName tyInfix count
      elseExpr = makeInstrElseExpr varName typeStr
  in makeIfElse cond ifExpr elseExpr

-- given a function id as "__VERIFIER_nondet_int"
grabTypeStr :: String -> String
grabTypeStr funCall = takeWhile (/= '(') $ last $ splitOn "_" funCall

makeTypeSpec :: String -> CDeclSpec
makeTypeSpec ty
  | ty == "int" = CTypeSpec $ CIntType undefNode
  | ty == "char" = CTypeSpec $ CCharType undefNode
  | ty == "short" = CTypeSpec $ CShortType undefNode
  | ty == "long" = CTypeSpec $ CLongType undefNode
  | ty == "float" = CTypeSpec $ CFloatType undefNode
  | ty == "double" = CTypeSpec $ CDoubleType undefNode
  | ty == "uint" = CTypeSpec $ CUnsigType undefNode
  | ty == "ulong" = CTypeSpec $ CLongType undefNode  
  | ty == "bool" = CTypeSpec $ CBoolType undefNode
  | ty == "pointer" = CTypeSpec $ CVoidType undefNode
  | otherwise = error $ "I don't know how to make type spec for: "++ty

isBasicType :: CDeclSpec -> Bool
isBasicType (CTypeSpec (CIntType _)) = True
isBasicType (CTypeSpec (CUnsigType _)) = True
isBasicType (CTypeSpec (CCharType _)) = True
isBasicType (CTypeSpec (CShortType _)) = True
isBasicType (CTypeSpec (CLongType _)) = True
isBasicType (CTypeSpec (CFloatType _)) = True
isBasicType (CTypeSpec (CDoubleType _)) = True
isBasicType (CTypeSpec (CBoolType _)) = True
isBasicType (CTypeSpec (CVoidType _)) = True
isBasicType _ = False

inferTypeStr :: CDeclSpec -> String
inferTypeStr (CTypeSpec (CIntType _)) = "int"
inferTypeStr (CTypeSpec (CUnsigType _)) = "uint"
inferTypeStr (CTypeSpec (CCharType _)) = "char"
inferTypeStr (CTypeSpec (CShortType _)) = "short"
inferTypeStr (CTypeSpec (CLongType _)) = "long"
inferTypeStr (CTypeSpec (CFloatType _)) = "float"
inferTypeStr (CTypeSpec (CDoubleType _)) = "double"
inferTypeStr (CTypeSpec (CBoolType _)) = "bool"
inferTypeStr (CTypeSpec (CVoidType _)) = "pointer"
--inferTypeStr _ = "pointer" -- trying to get SeaHorn-processable files
inferTypeStr tSpec = error $ "I can't infer the type string for: "++(show tSpec)
  
inferTypeSpec :: String -> CDeclSpec
inferTypeSpec funCall = (makeTypeSpec . grabTypeStr) funCall

verifierErrorCall :: CExpr
verifierErrorCall = (CCall (makeVar "__VERIFIER_error") [] undefNode)

grabAssignLhs :: CExpr -> CExpr
grabAssignLhs aExpr =
  let (CAssign CAssignOp lhs _ _) = aExpr
  in lhs

grabReadCallTypeSpec :: CExpr -> CDeclSpec
grabReadCallTypeSpec expr = inferTypeSpec (grabFuncCallId expr)

grabFuncCallId :: CExpr -> String
grabFuncCallId (CAssign CAssignOp _ (CVar (Ident iden _ _) _) _) = iden
grabFuncCallId (CAssign CAssignOp _ funcCall _) =
  let (CCall (CVar (Ident iden _ _) _) _ _) = funcCall
  in iden
grabFuncCallId (CCall (CVar (Ident iden _ _) _) _ _) = iden
grabFuncCallId e = error $ "I can't grab the function identifier for: "++(show e)

intType :: CDeclSpec
intType = CTypeSpec (CIntType undefNode)

makeGlobalQuadruples :: InputRead -> [CExtDecl]
makeGlobalQuadruples (InputRead idNum typeSpec _) =
  let iden = show idNum
      var = acaVarPrefix ++ (typeInfix typeSpec) ++ iden
      numVar = acaNumPrefix ++ iden
      counterVar = acaCounterPrefix ++ iden
      varArr = acaArrPrefix ++ (typeInfix typeSpec) ++ iden ++ "[0]"
      decls = [
        (declareVar typeSpec var),
        (declareVarInit intType numVar "0"),
        (declareVarInit intType counterVar "0"),
        (declareVar typeSpec varArr)
        ]
  in map CDeclExt decls

typeInfix :: CDeclSpec -> String
typeInfix s = (inferTypeStr s) ++ "_"

data CFunction = CFunction {
  name :: String,
  body :: CStat
  }
  
voidSp :: CDeclSpec
voidSp = CTypeSpec (CVoidType undefNode)

emptyStatement :: CStat
emptyStatement = CExpr Nothing undefNode

emptyB :: CBlockItem
emptyB = CBlockStmt emptyStatement

compoundEmpty :: CStat
compoundEmpty = CCompound [] [emptyB] undefNode

blockSubspaceId :: Ident
blockSubspaceId = Ident "block_subspace()" 0 undefNode

blockSubspaceDeclr :: CDeclr
blockSubspaceDeclr = CDeclr (Just blockSubspaceId) [] Nothing [] undefNode

blockSubspaceFunc :: CFunDef
blockSubspaceFunc = CFunDef [voidSp] blockSubspaceDeclr [] compoundEmpty undefNode

forDecl :: CDecl
forDecl = declareVarInit intType "i" "0"

forComparison :: Int -> CExpr
forComparison n = makeLtExpr "i" $ acaNumPrefix ++ (show n)

forIncrement :: CExpr
forIncrement = makeVar "i++"

deriveType :: String -> String
deriveType "void" = "void *"
deriveType _ = "int"

forBody :: Bool -> Int -> String -> CStat
forBody isCivl n ty =
  let ty'  = typeTag ty
      tmpName = "tmp_" ++ acaArrPrefix ++ ty' ++ "_" ++ (show n)
      initV = varInit ((deriveType ty)++" "++tmpName) ("__VERIFIER_nondet_" ++ ty' ++ "()")
      blockExpr1 = CBlockDecl initV
      idxVarName = acaArrPrefix ++ ty' ++ "_" ++ (show n) ++ "[i]"
      expr = assignExpr idxVarName tmpName
      expr2 = CExpr (Just expr) undefNode
      blockExpr2 = CBlockStmt expr2
  in
    if (isCivl && (isBounded ty'))
      then
        let maxB = maxAssumption idxVarName (maxValue ty')
            minB = minAssumption idxVarName (minValue ty')
            block = CCompound [] [blockExpr1, blockExpr2, maxB, minB] undefNode
        in block
      else CCompound [] [blockExpr1, blockExpr2] undefNode

isBounded :: String -> Bool
isBounded s = s `elem` ["char", "int", "long", "short", "uchar", "uint", "ulong", "ushort"]

maxValue :: String -> Integer
maxValue "char" = 127
maxValue "int" = 1073741823
maxValue "long" = 1073741823
maxValue "short" = 32767
maxValue "uchar" = 255
maxValue "uint" = 1073741823
maxValue "ulong" = 1073741823
maxValue "ushort" = 65535
maxValue t = error $ "Unaccounted for type: "++t

minValue :: String -> Integer
minValue "char" = 127
minValue "int" = 1073741823
minValue "long" = 1073741823
minValue "short" = 32768
minValue "uchar" = 0
minValue "uint" = 0
minValue "ulong" = 0
minValue "ushort" = 0
minValue t = error $ "Unaccounted for type: "++t

maxAssumption :: String -> Integer -> CBlockItem
maxAssumption varName val =
  let exprStr = "__VERIFIER_assume(("++varName++" - "++(show val)++") <= 0)"
      expr = makeVar exprStr
  in CBlockStmt (CExpr (Just expr) undefNode)

minAssumption :: String -> Integer -> CBlockItem
minAssumption varName val =
  let exprStr = "__VERIFIER_assume(0 <= ("++varName++" + "++(show val)++"))"
      expr = makeVar exprStr
  in CBlockStmt (CExpr (Just expr) undefNode)

forStatement :: Bool -> Int -> String -> CStat
forStatement isCivl n ty = CFor (Right forDecl) (Just (forComparison n)) (Just forIncrement)
                      (forBody isCivl n ty) undefNode

typeTag :: String -> String
typeTag "_Bool" = "bool"
typeTag "unsigned" = "uint"
typeTag "void" = "pointer"
typeTag ty = ty

cZero :: CInteger
cZero = cInteger 0

notEqualsZero :: CExpr -> CExpr
notEqualsZero e = CBinary CNeqOp e (CConst (CIntConst cZero undefNode)) undefNode

functionNameIs :: String -> CDeclr -> Bool
functionNameIs s declr =
  let (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
  in s == n

functionIs :: String -> CExtDecl -> Bool
functionIs s (CFDefExt (CFunDef _ declr _ _ _)) = functionNameIs s declr
functionIs _ _ = False

functionNameStartsWith :: String -> CDeclr -> Bool
functionNameStartsWith s declr =
  let (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
  in startswith s n

collectFunctions :: CTranslUnit -> [CExtDecl]
collectFunctions (CTranslUnit es _) = filter isFunction es

grabFunction :: CTranslUnit -> String -> Maybe CExtDecl
grabFunction ast f = find (functionIs f) (collectFunctions ast)

returnSuccess :: CBlockItem
returnSuccess =
  let zeroConst = CConst (CIntConst cZero undefNode)
      ret = CReturn (Just zeroConst) undefNode
  in CBlockStmt ret

isMain :: CExtDecl -> Bool
isMain (CFDefExt (CFunDef _ declr _ _ _)) = functionNameIs "main" declr
isMain _ = False

notMainOrModularFunction :: String -> CExtDecl -> Bool
notMainOrModularFunction funcName e = ((not . isMain) e) && ((not . (functionIs funcName)) e)

structName :: CStructUnion -> Maybe String
structName (CStruct _ Nothing _ _ _) = Nothing
structName (CStruct _ (Just (Ident n _ _)) _ _ _) = Just n

getParams :: CFunDef -> [CDecl]
getParams f =
  let (CFunDef _ (CDeclr _ paramList _ _ _) _ _ _) = f
      ps = head paramList
      (CFunDeclr (Right (params, _)) _ _) = ps
  in params

makeVarSymbolic :: Map.Map String CDecl -> (Map.Map String CStructUnion) -> (Map.Map String (CTypeSpec,Bool)) -> String -> IO [CBlockItem]
makeVarSymbolic nonlocalMap structMap typedefMap v = do
  let maybeD = Map.lookup v nonlocalMap
  if isNothing maybeD
    then error "variable not found in nonlocalMap"
    else do
      let (Just d) = maybeD
      makeDeclSymbolic d structMap typedefMap

makeBasicSym :: CDecl -> IO [CBlockItem]
makeBasicSym d@(CDecl ts _ _) = do
  let ty = inferTypeStr (head ts)
      v = extractDeclVarStr d
      expr = assignExpr v ("__VERIFIER_nondet_"++ty++"()")
      stmt = (CExpr (Just expr) undefNode)
  return [(CBlockStmt stmt)]
--makeBasicSym (CDecl _ _ _) = error $ "no support for making a basic type with multiple CDeclSpecs"
makeBasicSym _ = error $ "no support for making a basic type for a CStaticAssert"

makePtrToBasicSym :: CDecl -> IO [CBlockItem]
makePtrToBasicSym d@(CDecl [t] _ _) = do
  let ty = inferTypeStr t
  if (ty == "pointer")
    then do
      putStrLn "! Warning: symbolic pointer given a value of 0 -> analysis may be UNSOUND"
      putStrLn "(ALPACA does not support reasoning over generic pointers)"
      let pointerV = extractDeclVarStr d
          lhs = "*"++pointerV
          rhs = "0"
          decl = declareVarInit intType lhs rhs
      return [CBlockDecl decl]
    else do
      let pointerV = extractDeclVarStr d
          v = "__aca_val_"++pointerV
          decl = declareVarInit t v ("__VERIFIER_nondet_"++ty++"()")
          expr2 = assignExpr pointerV ("&"++v)
          stmt2 = (CExpr (Just expr2) undefNode)
      return [(CBlockDecl decl), (CBlockStmt stmt2)]
makePtrToBasicSym d = error $ "do not know how to make this pointer type symbolic: "++(show $ pretty d)  

makePtrToStructSym :: CDecl -> Map.Map String CStructUnion -> IO [CBlockItem]
makePtrToStructSym d structMap = do
  let pointerV = extractDeclVarStr d
      v = "__aca_val_"++pointerV
      (CDecl [ty] _ _) = d
      (CTypeSpec (CSUType (CStruct tag mId _ p1 p2) p3)) = ty
      ty' = (CTypeSpec (CSUType (CStruct tag mId Nothing p1 p2) p3))
      objDecl = (CBlockDecl (declareVar ty' v))

  structDef <- getStructDef d structMap
  let (CStruct _ _ (Just fieldDecls) _ _) = structDef

  setupAssigns <- mapM setupFieldAssign fieldDecls
  let assignExprs = map assignFieldToSymbolic fieldDecls

  let decl = CDecl [ty'] [] undefNode
      initList = map (\e->([],(CInitExpr e undefNode))) assignExprs
      compoundLit = CCompoundLit decl initList undefNode
  let assignE = CAssign CAssignOp (makeVar v) compoundLit undefNode
      assignStmt = CExpr (Just assignE) undefNode

      expr2 = assignExpr pointerV ("&"++v)
      stmt2 = (CExpr (Just expr2) undefNode)
  return $ [objDecl]++setupAssigns++[(CBlockStmt assignStmt), (CBlockStmt stmt2)]

makeStructSym :: CDecl -> Map.Map String CStructUnion -> IO [CBlockItem]
makeStructSym d structMap = do
  let v = extractDeclVarStr d
  structDef <- getStructDef d structMap
  let (CStruct _ _ (Just fieldDecls) _ _) = structDef

  setupAssigns <- mapM setupFieldAssign fieldDecls
  let assignExprs = map assignFieldToSymbolic fieldDecls

  let (CDecl [ty] _ _) = d
      (CTypeSpec (CSUType (CStruct tag mId _ p1 p2) p3)) = ty
      ty' = (CTypeSpec (CSUType (CStruct tag mId Nothing p1 p2) p3))
  let decl = CDecl [ty'] [] undefNode
      initList = map (\e->([],(CInitExpr e undefNode))) assignExprs
      compoundLit = CCompoundLit decl initList undefNode
  let assignE = CAssign CAssignOp (makeVar v) compoundLit undefNode
      assignStmt = CExpr (Just assignE) undefNode

  return $ setupAssigns++[(CBlockStmt assignStmt)]

makeTypeDefSym :: CDecl -> Map.Map String CStructUnion -> (Map.Map String (CTypeSpec,Bool)) -> IO [CBlockItem]
makeTypeDefSym (CDecl [(CTypeSpec (CTypeDef (Ident n _ _) _))] p1 p2) structMap typedefMap = do
  let maybeT = Map.lookup n typedefMap
  if isJust maybeT
    then do
      let (Just (t, isPtr)) = maybeT
      if isTypeStruct t
        then do
          let structN = structTypeName t
              maybeS = Map.lookup structN structMap
          if isJust maybeS
            then do
              let (Just s) = maybeS
                  d' = CDecl [(CTypeSpec (CSUType s undefNode))] p1 p2
              if isPtr
                then makePtrToStructSym d' structMap
                else makeStructSym d' structMap
            else error $ "could not find "++structN++" in structMap"
      else do
          let d' = CDecl [(CTypeSpec t)] p1 p2
          if isPtr
            then makePtrToBasicSym d'
            else makeBasicSym d'
    else error $ "could not find "++n++" in the map of typedefs"
makeTypeDefSym d _ _ = error $ "unable to make this typedef symbolic: "++(show $ pretty d)

declType :: CDecl -> IO DeclType
declType (CDecl ts [((Just (CDeclr _ derived _ _ _)),_,_)] _) = do
  if isBasicType (head ts)
    then if (null derived) then return Basic else return PtrToBasic
    else if (isTypeDef (head ts))
           then return TypeDef
           else if (null derived) then return Struct else return PtrToStruct
declType d = error $ "did not account for this decl type: "++(show d)

isTypeDef :: CDeclSpec -> Bool
isTypeDef (CTypeSpec (CTypeDef _ _)) = True
isTypeDef _ = False
                                                   
data DeclType = Basic | PtrToBasic | Struct | PtrToStruct | TypeDef

makeDeclSymbolic :: CDecl -> Map.Map String CStructUnion -> (Map.Map String (CTypeSpec,Bool)) -> IO [CBlockItem]
makeDeclSymbolic d structMap typedefMap = do
  dType <- declType d
  case dType of Basic -> makeBasicSym d
                PtrToBasic -> makePtrToBasicSym d
                Struct -> makeStructSym d structMap
                PtrToStruct -> makePtrToStructSym d structMap
                TypeDef -> makeTypeDefSym d structMap typedefMap -- grab the typedef alias from d; then call makeDeclSymbolic again
                       
setupFieldAssign :: CDecl -> IO CBlockItem
setupFieldAssign (CDecl ss [((Just cDeclr),_,_)] _) = do
  let ty = inferTypeStr (head ss)
  rhs <- if (ty == "pointer")
           then do
             putStrLn "! Warning: symbolic pointer given a value of 0 -> analysis may be UNSOUND"
             putStrLn "(ALPACA does not support reasoning over generic pointers)"
             return "0"
           else return $ "__VERIFIER_nondet_"++ty++"()"
  let e = makeExpr rhs
      decl = (CDecl ss [((Just cDeclr),(Just (CInitExpr e undefNode)),Nothing)] undefNode)
  return $ CBlockDecl decl
setupFieldAssign d = error $ "I don't know how to set up this field assignment: "++(show $ d)

assignFieldToSymbolic :: CDecl -> CExpr
assignFieldToSymbolic (CDecl _ [((Just cDeclr),_,_)] _) =
  let (CDeclr (Just (Ident n _ _)) _ _ _ _) = cDeclr
      e = assignExpr ("."++n) n
  in e
assignFieldToSymbolic _ = error "I do not know how to make this symbolic assign expression."

getStructDef :: CDecl -> (Map.Map String CStructUnion) -> IO CStructUnion
getStructDef d structMap = do
  let (CDecl [(CTypeSpec t)] _ _) = d
      isStruct = isTypeStruct t
  if isStruct
    then do
      let structN = structTypeName t
          maybeDef = Map.lookup structN structMap
      if isJust maybeDef
        then do
          let (Just def) = maybeDef
          return def
        else error "struct def is not in struct map"
    else error "variable is not a struct"

structTypeName :: CTypeSpec -> String
structTypeName (CSUType (CStruct _ (Just (Ident structN _ _)) _ _ _) _) = structN
structTypeName s = error $ "this is not a struct type: "++(show $ pretty s)

isTypeStruct :: CTypeSpec -> Bool
isTypeStruct (CSUType _ _) = True
isTypeStruct _ = False

nonvoidDecl :: CDecl -> Bool
nonvoidDecl (CDecl [(CTypeSpec (CVoidType _))] _ _) = False
nonvoidDecl _ = True

symbolicSetup :: [CDecl] -> [String] -> Map.Map String CDecl -> (Map.Map String CStructUnion) -> (Map.Map String (CTypeSpec,Bool)) -> IO [CBlockItem]
symbolicSetup ds vs nonlocalMap structMap typedefMap= do
  let ds' = filter nonvoidDecl ds
      paramDecls = map (\d->(CBlockDecl d)) ds'
      paramNames = map extractDeclVarStr ds'
      vs' = nub $ paramNames ++ vs
  stmts <- mapM (makeVarSymbolic nonlocalMap structMap typedefMap) vs'
  let stmts' = concat stmts
  return $ paramDecls++stmts'

hasFloat128 :: CExtDecl -> Bool
hasFloat128 (CDeclExt d) = "_Float128" `isInfixOf` (show $ pretty d)
hasFloat128 _ = False

-- we remove main
-- assuming no re-entrant code (calling main from multiple places)
-- throw away external declarations that involve _Float128
--   (many underlying tools cannot deal with this)

makeModularAst :: CTranslUnit -> String -> IO CTranslUnit
makeModularAst ast@(CTranslUnit es _) funcName = do
  let func = grabFunction ast funcName
      es' = filter (not . hasFloat128) es
  if isJust func
    then do
      let (Just (CFDefExt f)) = func
      structMap <- collectStructMap es'
      typedefMap <- collectTypeDefMap es'
      let params = getParams f
      nonlocalMap <- collectNonlocalMap params es'

      let (CFunDef _ _ _ funcBody _) = f
          varIdsReferenced = extractStmtVars funcBody
          varsOfInterest = intersect (Map.keys nonlocalMap) varIdsReferenced

      setup <- symbolicSetup params varsOfInterest nonlocalMap structMap typedefMap
      let callStmt = makeCallStmt funcName params
          
      let nonMainDefs = filter (not . isMain) es'
          body' = (CCompound [] (setup++[(CBlockStmt callStmt)]) undefNode)
          newMain = (CFDefExt (CFunDef [intType] (makeDeclarator "main()") [] body' undefNode))
          ast' = (CTranslUnit (nonMainDefs ++ [newMain]) undefNode)
      return ast'
      
    else return (assert False ast) -- given function not in AST

makeCallStmt :: String -> [CDecl] -> CStat
makeCallStmt funcName params = 
  let paramVars = map (makeVar . extractDeclVarStr) params
      funcCall = CCall (makeVar funcName) paramVars undefNode
      stmt = CExpr (Just funcCall) undefNode
  in stmt

collectStructs :: [CExtDecl] -> IO [CStructUnion]
collectStructs es = do
  let mStructs = map getStruct es
      structs = catMaybes mStructs
  return structs

collectNonlocalMap :: [CDecl] -> [CExtDecl] -> IO (Map.Map String CDecl)
collectNonlocalMap params es = do
  let paramTuples = map (\d -> ((extractDeclVarStr d),d)) params

  let globalExtDecls = filter keepExtDecl es
      globalDecls = map (\(CDeclExt d)->d) globalExtDecls
      globalDecls' = filter hasVarName globalDecls
      globalDecls'' = filter startsWithTypeSpec globalDecls'
      globalTuples = map (\d -> ((extractDeclVarStr d),d)) globalDecls''

  let m = Map.fromList (globalTuples++paramTuples)
  return m

keepExtDecl :: CExtDecl -> Bool
keepExtDecl (CDeclExt _) = True
keepExtDecl _ = False

hasVarName :: CDecl -> Bool
hasVarName (CDecl _ exprs _) = not $ null exprs
hasVarName _ = False

startsWithTypeSpec :: CDecl -> Bool
startsWithTypeSpec (CDecl [(CTypeSpec _)] _ _) = True
startsWithTypeSpec _ = False

getTypeDef :: CExtDecl -> Maybe (String, (CTypeSpec,Bool))
getTypeDef (CDeclExt d@(CDecl [(CStorageSpec (CTypedef _)),(CTypeSpec ty)] [((Just (CDeclr _ derived _ _ _)),_,_)] _)) =
  if null derived
    then Just ((extractDeclVarStr d),(ty,False))
    else Just ((extractDeclVarStr d),(ty,True))
getTypeDef _ = Nothing

collectTypeDefs :: [CExtDecl] -> IO [(String, (CTypeSpec,Bool))]
collectTypeDefs es = do
  let mTypeDefs = map getTypeDef es
      typedefs = catMaybes mTypeDefs
  return typedefs

-- the Bool says whether the type def is derived or not
collectTypeDefMap :: [CExtDecl] -> IO (Map.Map String (CTypeSpec,Bool))
collectTypeDefMap es = do
  typedefs <- collectTypeDefs es
  return $ Map.fromList typedefs

collectStructMap :: [CExtDecl] -> IO (Map.Map String CStructUnion)
collectStructMap es = do
  structs <- collectStructs es

  let names = map structName structs
      tuples = zip names structs
      tuples' = filter hasName tuples
      tuples'' = map (\((Just n),d)->(n,d)) tuples'
  return $ Map.fromList tuples''

hasName :: (Maybe String, a) -> Bool
hasName ((Just _),_) = True
hasName _ = False

getStruct :: CExtDecl -> Maybe CStructUnion
getStruct (CDeclExt (CDecl [CTypeSpec (CSUType s@(CStruct CStructTag _ _ _ _) _)] _ _)) = Just s
getStruct (CDeclExt (CDecl [_, (CTypeSpec (CSUType s@(CStruct CStructTag _ _ _ _) _))] _ _)) = Just s
getStruct _ = Nothing

makeGlobalVarSymbolic :: Map.Map String CStructUnion -> CExtDecl -> Maybe CDecl
makeGlobalVarSymbolic _ (CDeclExt decl) =
  -- just need to adjust this statement, given Will's test case
  if declHasSingleNameAndType decl
    then Just (makeGlobalSymbolic decl)
    else Nothing
makeGlobalVarSymbolic _ _ = Nothing

declHasSingleNameAndType :: CDecl -> Bool
declHasSingleNameAndType (CDecl _ [((Just _), Nothing, Nothing)] _) = True
declHasSingleNameAndType (CDecl _ [((Just _), _, Nothing)] _) = True
declHasSingleNameAndType _ = False

-- 3) Parameter declarations (K&R A8.6.3, C99 6.7.5 parameter-declaration)
--
--   * @init-declarator-list@ must contain at most one triple of the form @(Just declr, Nothing, Nothing)@,
--     i.e. consist of a single declarator, which is allowed to be abstract (i.e. unnamed).
makeSymbolic :: Map.Map String CStructUnion -> CDecl -> IO CDecl
{- Need to take care to two different cases:
  - basic types
  - structured types
  - (deal with pointers after)
-}
-- assume just a type with a name for now
makeSymbolic _ (CDecl [ty] [((Just declr), Nothing, Nothing)] _) = do
  if isBasicType ty
    then do
      let suffix = inferTypeStr ty
          (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
          ini = declareVarInit ty n ("__VERIFIER_nondet_"++suffix++"()")
      return ini
    else error "need to implement making a struct symbolic"
makeSymbolic _ d = return (assert False d)

makeGlobalSymbolic :: CDecl -> CDecl
-- assume just a type with a name for now
makeGlobalSymbolic (CDecl [ty] [((Just declr), _, Nothing)] _) =
  let suffix = inferTypeStr ty
      (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
      ini = varInit n ("__VERIFIER_nondet_"++suffix++"()")
  in ini
makeGlobalSymbolic d = d

isFunction :: CExtDecl -> Bool
isFunction (CFDefExt (CFunDef _ _ _ _ _)) = True
isFunction _ = False

extractStmtVars :: CStat -> [String]
extractStmtVars (CLabel _ s _ _) = extractStmtVars s
extractStmtVars (CCase e s _) = (extractExprVars e)++(extractStmtVars s)
extractStmtVars (CCases e1 e2 s _) = (extractExprVars e1)++(extractExprVars e2)++(extractStmtVars s)
extractStmtVars (CDefault s _) = extractStmtVars s
extractStmtVars (CExpr Nothing _) = []
extractStmtVars (CExpr (Just e) _) = extractExprVars e
extractStmtVars (CCompound _ bs _) = concat $ map processBlockStmt bs
extractStmtVars (CIf e s Nothing _) = (extractExprVars e)++(extractStmtVars s)
extractStmtVars (CIf e s1 (Just s2) _) = (extractExprVars e)++(extractStmtVars s1) ++(extractStmtVars s2)
extractStmtVars (CSwitch e s _) = (extractExprVars e)++(extractStmtVars s)
extractStmtVars (CWhile e s _ _) = (extractExprVars e)++(extractStmtVars s)
extractStmtVars (CFor (Left mE1) mE2 mE3 s _) = (varsMExpr mE1)++(varsMExpr mE2)++(varsMExpr mE3)++(extractStmtVars s)
extractStmtVars (CFor _ mE2 mE3 s _) = (varsMExpr mE2)++(varsMExpr mE3)++(extractStmtVars s)
extractStmtVars (CReturn (Just e) _) = extractExprVars e
extractStmtVars _ = []

varsMExpr :: Maybe CExpr -> [String]
varsMExpr Nothing = []
varsMExpr (Just e) = extractExprVars e
  
processBlockStmt :: CBlockItem -> [String]
processBlockStmt (CBlockStmt s) = extractStmtVars s
processBlockStmt _ = []

extractExprVars :: CExpr -> [String]
extractExprVars (CComma es _) = concat $ map extractExprVars es
extractExprVars (CAssign _ lhs rhs _) = (extractExprVars lhs)++(extractExprVars rhs)
extractExprVars (CCond e1 mE2 e3 _) = (extractExprVars e1)++(varsMExpr mE2)++(extractExprVars e3)
extractExprVars (CBinary _ e1 e2 _) = (extractExprVars e1)++(extractExprVars e2)
extractExprVars (CUnary _ e _) = extractExprVars e
extractExprVars (CIndex e1 e2 _) = (extractExprVars e1)++(extractExprVars e2)
extractExprVars (CCall e1 es _) = (extractExprVars e1)++(concat $ map extractExprVars es)
extractExprVars (CMember e _ _ _) = (extractExprVars e)
extractExprVars (CVar (Ident n _ _) _) = [n]
extractExprVars _ = []
\end{code}
