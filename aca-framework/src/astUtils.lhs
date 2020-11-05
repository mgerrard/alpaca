\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Control.Monad
import Data.Maybe

import Language.C
--import Language.C.DSL
import Language.C.Analysis   
import Language.C.System.GCC
import Language.C.Syntax.AST
import Language.C.Data.Ident

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

getAst = parseMyFile "test.c"

funDefs :: [CExtDecl] -> [CFunDef]
funDefs [] = []
funDefs ((CFDefExt x):xs) = x:funDefs xs
funDefs (x:xs) = funDefs xs

isName :: String -> CFunDef -> Bool
isName s fun =
  let (CFunDef _ decl _ _ _) = fun
      (CDeclr (Just ident) _ _ _ _) = decl
      (Ident idStr _ _) = ident
  in idStr == s

grabFuncByName :: String -> CTranslUnit -> CFunDef
grabFuncByName s ast =
  let (CTranslUnit decls _) = ast
      name = isName s in
    head $ filter name $ funDefs decls

isMainFunction :: CExtDecl -> Bool
isMainFunction d
  | (CFDefExt fn) <- d = isName "main" fn
  | otherwise          = False

nonMainDecls :: CTranslUnit -> [CExtDecl]
nonMainDecls ast =
  let (CTranslUnit decls _) = ast in
    filter (not . isMainFunction) decls

instrument :: CTranslUnit -> CTranslUnit
instrument ast =
  let unmodifiedDecls = nonMainDecls ast
      origMain = grabFuncByName "main" ast
      newMain = prependToFunc prefix origMain
      newDecls = map export [initializeReads,
                             constrainSpace ["constraints"],
                             newMain]
  in
    transUnit $ unmodifiedDecls ++ newDecls

-- Placeholder hardcoded prefix
{-
prefix :: CStat
prefix = block [
  intoB $ "initializeReads"#[],
  intoB $ "constrainSpace"#[]
  ]
-}

-- Function to find any globals referenced in a C function

-- Function to extract the parameters from a C function
extractParameters :: CFunDef -> [CDecl]
extractParameters f =
  let (CFunDef _ declr _ _ _) = f
      (CDeclr _ funDeclr _ _ _) = declr
      (CFunDeclr x _ _) = head funDeclr
      Right (declArr,_) = x
  in declArr

extractTypeIdentPair :: CDecl -> (CTypeSpec, Ident)
extractTypeIdentPair decl =
  let (CDecl declSpec declaratorArr _) = decl
      (Just declarator, _, _) = head declaratorArr
      (CDeclr (Just id) _ _ _ _) = declarator
      (CTypeSpec typeSpec) = head declSpec
  in (typeSpec, id)

extractName :: Ident -> String
extractName id =
  let (Ident name _ _) = id in name

-- Function to intialize each typed variable in array with fresh typed read

-- type id = __VERIFIER_nondet_type();

--freshReadStr :: String -> String ->

type Formula = String
type Formulae = [Formula]

{-
initializeReads :: CFunDef
initializeReads =
  fun [voidTy] "initializeReads"[] $ hBlock [
    liftE $ "printf"#[str"Initializing reads\n"]
  ]

constrainSpace :: Formulae -> CFunDef
constrainSpace fs =
  fun [voidTy] "constrainSpace"[] $ hBlock [
    liftE $ "printf"#[str"Constraining state space\n"]
  ]
  
toplevel :: CFunDef -> CTranslUnit
toplevel m = transUnit [
  export initializeReads,
  export $ constrainSpace ["constraints"],
  export m
  ]
-}

prependToFunc :: CStat -> CFunDef -> CFunDef
prependToFunc pre f =
  let (CFunDef p1 p2 p3 stmt p5) = f
      origBody = CBlockStmt stmt
      prefix = CBlockStmt pre
      newBody = [prefix, origBody]
      newCStmt = CCompound [] newBody undefNode
      newMain = CFunDef p1 p2 p3 newCStmt p5
  in newMain
      
-- AST rewrite functions

type ExprRewrite = (CExpr, CExpr)

rewriteExt :: CExtDecl -> ExprRewrite -> CExtDecl
rewriteExt (CFDefExt funDef) e = CFDefExt $ rewriteFun funDef e
rewriteExt identity _ = identity

rewriteFun :: CFunDef -> ExprRewrite -> CFunDef
rewriteFun (CFunDef p1 p2 p3 s p5) e = CFunDef p1 p2 p3 (rewriteStmt s e) p5

rewriteStmt :: CStat -> ExprRewrite -> CStat
rewriteStmt (CLabel p1 s p3 p4) e = CLabel p1 (rewriteStmt s e) p3 p4
rewriteStmt (CCase p1 s p3) e = CCase p1 (rewriteStmt s e) p3
rewriteStmt (CCases p1 p2 s p4) e = CCases p1 p2 (rewriteStmt s e) p4
rewriteStmt (CDefault s p2) e = CDefault (rewriteStmt s e) p2
rewriteStmt (CExpr (Just expr) p2) e = CExpr (Just (rewriteExpr expr e)) p2
rewriteStmt (CExpr Nothing p2) e = CExpr Nothing p2
rewriteStmt (CCompound p1 cBlock p3) e = CCompound p1 (rewriteCBlock cBlock e) p3
rewriteStmt (CIf p1 s1 (Just s2) p4) e = CIf p1 (rewriteStmt s1 e) (Just (rewriteStmt s2 e)) p4
rewriteStmt (CIf p1 s1 Nothing p4) e = CIf p1 (rewriteStmt s1 e) Nothing p4
rewriteStmt (CSwitch p1 s p3) e = CSwitch p1 (rewriteStmt s e) p3
rewriteStmt (CWhile p1 s p3 p4) e = CWhile p1 (rewriteStmt s e) p3 p4
rewriteStmt (CFor p1 p2 p3 s p5) e = CFor p1 p2 p3 (rewriteStmt s e) p5
rewriteStmt identity e = identity

rewriteCBlock :: [CBlockItem] -> ExprRewrite -> [CBlockItem]
rewriteCBlock block e = map (rewriteBlockItem e) block

rewriteBlockItem :: ExprRewrite -> CBlockItem -> CBlockItem
rewriteBlockItem e (CBlockStmt stmt) = CBlockStmt (rewriteStmt stmt e)
rewriteBlockItem _ identity = identity

rewriteExpr :: CExpr -> ExprRewrite -> CExpr
rewriteExpr e rw =
  let target  = fst rw
      replace = snd rw
  in
    -- TODO: extend AST to make CExpr derive Eq so we
    -- won't compare against strings
    if (show e) == (show target) then replace else e

bazId = Ident "baz" 0 undefNode
baz = CVar bazId undefNode

fortyTwoInt = CIntConst (cInteger 42) undefNode
fortyTwo = CConst fortyTwoInt

testExpr = CAssign CAssignOp baz fortyTwo undefNode

-- Simple case for now: assign inside a function definition
grabExprs :: CTranslUnit -> [CExpr]
grabExprs ast =
  let (CTranslUnit extDecls _) = ast
      funs = funDefs extDecls
      bodies = map getBody funs
  in
    concat $ map exprs bodies

grabBodies :: CTranslUnit -> [CStat]
grabBodies ast =
  let (CTranslUnit extDecls _) = ast
      funs = funDefs extDecls
  in map getBody funs
  
getBody :: CFunDef -> CStat
getBody (CFunDef _ _ _ b _) = b

exprs :: CStat -> [CExpr]
exprs (CExpr (Just e) _) = [e]
exprs (CCompound _ blockStmts _) =
  let stmts = concat $ map blockItemToStmt blockStmts in
    concat $ map exprs stmts
exprs id = []
    
--  concat $ map (exprs . catMaybes . blockItemToStmt) blockStmts

blockItemToStmt :: CBlockItem -> [CStat]
blockItemToStmt (CBlockStmt s) = [s]
blockItemToStmt _ = []

-- Grab all statements

-- Grab all expressions

-- Grab all the assign statements


-- Filter by line number and identifier
\end{code}
