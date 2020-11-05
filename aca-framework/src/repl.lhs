\begin{code}
import Control.Monad
import System.IO
import Language.C
import Language.C.System.GCC   -- preprocessor used
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Language.C.Syntax.Ops
import Data.List (find)
import Data.SBV
import qualified Data.Map as M

main :: IO ()
main = do
  putStrLn banner
  mainLoop

banner :: String
banner = "\n"
       ++"This is a program to test out the satisfiability\n"
       ++"of some proposition written in C. The user first\n"
       ++"inputs declarations of the variables used in the\n"
       ++"expression. Then the expression itself is input.\n"
       ++"(Note: decls end with semicolons; exprs do not.)\n"
       ++"\n************************************************\n"

mainLoop :: IO ()
mainLoop = do
  globals <- prompt "gimme variable declarations: "
  expr <- prompt "gimme a C expression: "

  cFile <- makeProgram globals expr
  expr <- parseMyExpr cFile
  let is = extractCIndexes expr
  putStrLn "Indexes:"
  putStrLn (show is)
  let env = ["x[0]","x[1]"] {- just a mockup -}
  result <- solvePred expr env
  putStrLn (show result)
  
  putStrLn ""  
  putStrLn "************************************************"
  putStrLn ""  
  mainLoop

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

{- The environment map will be from Strings to SIntegers,
to start with
 (look here for more general solution:
    wiki.haskell.org/Heterogenous_collections)

type Env = M.Map String SInteger

(see stackoverflow.com/questions/23233969)
-}

type Env = M.Map String SInteger

envLookup :: String -> Env -> SInteger
envLookup v e = maybe (error $ "Var not found: " ++ show v) id
                            (M.lookup v e)

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

solvePred :: CExpr -> [String] -> IO SatResult
solvePred e0 vs = sat pr
 where
  pr :: Predicate
  pr = do
      syms <- mapM exists vs
      let env = M.fromList (zip vs syms)
      interpret env e0

interpret :: Env -> CExpr -> Predicate
interpret _ (CConst c) = truthValue c
interpret env (CUnary CNegOp expr _) = do
  e <- interpret env expr
  return $ bnot e
interpret env (CBinary op lhs rhs _)
  | isCmpOp op = do
      let l = deriveNumericExpr env lhs
      let r = deriveNumericExpr env rhs
      return $ (sbvCompOp op) l r    
  | isLogicOp op = do
      l <- interpret env lhs
      r <- interpret env rhs
      return $ (sbvLogicOp op) l r
interpret _ _ = error "This expression type is not implemented."

deriveNumericExpr :: Env -> CExpr -> SInteger
deriveNumericExpr _ (CConst c) = literal (unpackedConst c)
deriveNumericExpr env (CIndex (CVar id _) (CConst (CIntConst const _)) _) =
  let const' = getCInteger const
      idxStr = show const'
      idStr = identToString id
      arrElem = idStr++"["++idxStr++"]"
  in envLookup arrElem env
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
deriveNumericExpr _ _ = error "This numeric expression type is not implemented."

truthValue :: CConst -> Predicate
truthValue c = do
  let val = unpackedConst c
  if val == 0
    then return false
    else return true

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

sbvLogicOp :: (Boolean b) => CBinaryOp -> (b -> b -> b)
sbvLogicOp CLndOp = (&&&)
sbvLogicOp CLorOp = (|||)
sbvLogicOp op = error $ "Not a boolean operator: "++(show op)

sbvPtrOp :: (Num b) => CBinaryOp -> (b -> b -> b)
sbvPtrOp CAddOp = (+)
sbvPtrOp CSubOp = (-)
sbvPtrOp op = error $ "Not an arithmetic operator: "++(show op)

makeProgram :: String -> String -> IO FilePath
makeProgram globals expr = do
  let progStr = globals++"int main(){"++expr++";}"
  let path = "/tmp/expr.c"
  writeFile path progStr
  return path

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

parseMyExpr :: FilePath -> IO CExpr
parseMyExpr input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return (collectExpr ast)

grabFunction :: CTranslUnit -> String -> Maybe CExtDecl
grabFunction ast@(CTranslUnit es _) f = find (functionIs f) (collectFunctions ast)

functionIs :: String -> CExtDecl -> Bool
functionIs s (CFDefExt (CFunDef _ declr _ _ _)) = functionNameIs s declr
functionIs _ _ = False

functionNameIs :: String -> CDeclr -> Bool
functionNameIs s declr =
  let (CDeclr (Just (Ident name _ _)) _ _ _ _) = declr
  in s == name

collectFunctions :: CTranslUnit -> [CExtDecl]
collectFunctions (CTranslUnit es _) = filter isFunction es

isFunction :: CExtDecl -> Bool
isFunction (CFDefExt (CFunDef _ _ _ _ _)) = True
isFunction _ = False

collectExpr :: CTranslUnit -> CExpr
collectExpr ast =
  let m = grabFunction ast "main"
      (Just
       (CFDefExt
        (CFunDef _ _ _
         (CCompound _ [blockItem] _) _))) = m
      (CBlockStmt (CExpr (Just expr) _)) = blockItem
  in expr

printMyAST :: CTranslUnit -> IO ()
printMyAST ctu = (print . pretty) ctu
\end{code}
