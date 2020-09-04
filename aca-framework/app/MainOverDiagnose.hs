module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import CscTypes
import Transformer
import Portfolio
import RunPortfolio
import LaunchBenchexec
import Transformations
import Analysis
import Writing
import Data.List (nub, maximumBy, groupBy, intercalate, partition)
import Data.List.Split (splitOn)
import System.Directory

import Language.C hiding (execParser)
import Language.C.Analysis   
import Language.C.System.GCC
import Language.C.Syntax.AST
import Language.C.Data.Ident

main :: IO ()
main = do
  runMain =<< execParser opts
  return ()
      
runMain :: Configuration -> IO ()
runMain (Configuration cscfile) = do
  csc <- readCsc cscfile
  let cs = problemConjuncts csc
  let pre = makePreamble cs
  let css = removeEmpty $ powerset cs
  {- we only want to run the overapproximators in these -}
  ds <- mapM (diagnose pre cs) (zip [0..] css)
  mapM_ printDiagnoses ds
  return ()

printDiagnoses :: OverapproxSummary -> IO ()
printDiagnoses (sub, rs) = do
  putStrLn "Trying to block the conjunction of:"
  mapM_ (putStrLn . ((++) "  ") . simplifyCee . ceeConjunct) sub
  let (falseRs, others) = partition (\(_, r) -> r==FalseResult) rs
  let (trueRs, unknowns) = partition (\(_, r)->r==TrueResult) others
  if (not (null falseRs))
    then do
      putStrLn "\n  Overapproximates the assumption:"
      mapM_ (\(n,_)->putStrLn $ "   "++n) falseRs
    else return ()
  if (not (null trueRs))
    then do
      putStrLn "\n  Respects the assumption:"
      mapM_ (\(n,_)->putStrLn $ "   "++n) trueRs
    else return ()
  if (not (null unknowns))
    then do
      putStrLn "\n  Returns an unknown result:"
      mapM_ (\(n,_)->putStrLn $ "   "++n) unknowns
    else return ()
  putStrLn "\n*****"

type OverapproxSummary = ([Conjunct], [(String, AnalysisResult)])

overapproximators :: [String]
overapproximators = [
  "cpaSeq", "cpaBamBnb", "cpaBamSlicing",
  "interpChecker", "uAutomizer", "uKojak",
  "uTaipan", "veriAbs", "depthK", "pesco"
  ]

diagnose :: [CExtDecl] -> [Conjunct] -> (Int, [Conjunct]) -> IO OverapproxSummary
diagnose pre cs (idx, sub) = do
  let progStr = show $ pretty $ makeProgram pre cs sub
  let tmpFile = "./diagnose_prog_"++(show idx)++".c"
  writeFile tmpFile progStr
  rs <- mapM (runTool tmpFile) overapproximators
  let summaries = zip overapproximators rs
  return (sub, summaries)

runTool :: FilePath -> String -> IO AnalysisResult
runTool cFile tool = do
  let t = 15 {- timeout in seconds -}
  setLibraryEnvironmentVariable
  as <- portfolio tool t t
  let a = head as
  let outDir = "./runtoolLogs/"++cFile
  removeDirectoryRecursiveIfExists outDir
  createDirectoryIfMissing True outDir
  {- The final three parameters are irrelevant;
     change the 4th if you want a different output dir -}
  (_, stdOut, _) <- runBenchexec cFile a t outDir Nothing False "."
  return (getResultSummary stdOut)

removeEmpty :: [[a]] -> [[a]]
removeEmpty = init

{- from LYAH -}
powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs

makeProgram :: [CExtDecl] -> [Conjunct] -> [Conjunct] -> CTranslUnit
makeProgram pre f subF =
  let m = makeMainBody f subF
  in CTranslUnit (pre++[m]) undefNode

makePreamble :: [Conjunct] -> [CExtDecl]
makePreamble cs = 
  let extDecls = makeExternalDecls
      ivs = nub $ concat $ map inputVars cs
      readsMap = toReadsMap ivs
      newDecls = map makeQuadruples readsMap
      initFunc = initializeReadsFunc readsMap
  in extDecls++(join newDecls)++[(CFDefExt initFunc)]

makeMainBody :: [Conjunct] -> [Conjunct] -> CExtDecl
makeMainBody f subF =
  let initCall = makeCCallBlockItem "initialize_reads"
      assumeCall = blockConjuncts subF
      ifBlockStmt = CBlockStmt (makeIfStmt f)
      mf = mainFunc [initCall, assumeCall, ifBlockStmt]
  in CFDefExt mf

blockConjuncts :: [Conjunct] -> CBlockItem
blockConjuncts cs =
  let prefix         = "__VERIFIER_assume(!("
      ceeConjunction = map ceeConjunct cs
      expr           = intercalate " && " ceeConjunction
      suffix         = "))"
      stmt           = (CExpr (Just (makeVar $ prefix <> expr <> suffix))) undefNode
  in CBlockStmt stmt

mainFunc :: [CBlockItem] -> CFunDef
mainFunc bs =
  let n = Ident "main()" 0 undefNode
      declr = CDeclr (Just n) [] Nothing [] undefNode
      mainBody = (CCompound [] bs undefNode)
  in CFunDef [intType] declr [] mainBody undefNode

makeIfStmt :: [Conjunct] -> CStat
makeIfStmt cs =
  let condExpr = makeConjExpr cs
  in CIf condExpr errorStmt Nothing undefNode

errorStmt :: CStat
errorStmt = (CExpr (Just verifierErrorCall) undefNode)  

makeConjExpr :: [Conjunct] -> CExpr
makeConjExpr cs =
  let ceeConjunction = map ceeConjunct cs
      expr           = intercalate " && " ceeConjunction
  in makeVar expr

{- represents (Id, Idx, Type) -}
type TransInput = (Int, Int, String)

toReadsMap :: [InputVariable] -> [InputRead]
toReadsMap vs =
  let vs' = map makeTransInput vs
      gs = groupBy (\(n1,_,_) (n2,_,_)->n1==n2) vs'
      ms = map (maximumBy (\(_,c1,_) (_,c2,_)->c1 `compare` c2)) gs
  in map (\(i, idx, ty)->(InputRead i intType (idx+1))) ms

makeTransInput :: InputVariable -> TransInput
makeTransInput (varName, cType) =
  let idStr = last $ init $ init $ splitOn "_" varName
      idxStr = last $ init $ splitOn "_" varName
      n = read idStr :: Int
      idx = read idxStr :: Int
  in (n, idx, cType)

problemConjuncts :: Csc -> [Conjunct]
problemConjuncts csc = conjuncts $ upper $ upperBound $ last $ disjointPartitions csc
        
{- Will only consider integer reads for now -}
makeExternalDecls :: [CExtDecl]
makeExternalDecls =
  [errorDecl, assumeDecl, nondetIntDecl]

nondetIntDecl :: CExtDecl
nondetIntDecl = 
  let n = Ident "__VERIFIER_nondet_int()" 0 undefNode
      declr = CDeclr (Just n) [] Nothing [] undefNode
      decl = CDecl [externType, intType] [(Just declr, Nothing, Nothing)] undefNode
  in CDeclExt decl

assumeDecl :: CExtDecl
assumeDecl =
  let n = Ident "__VERIFIER_assume(int)" 0 undefNode
      declr = CDeclr (Just n) [] Nothing [] undefNode
      decl = CDecl [externType, voidType] [(Just declr, Nothing, Nothing)] undefNode
  in CDeclExt decl

errorDecl :: CExtDecl
errorDecl =
  let n = Ident "__VERIFIER_error()" 0 undefNode
      declr = CDeclr (Just n) [] Nothing [] undefNode
      decl = CDecl [externType, voidType, attributeQual] [(Just declr, Nothing, Nothing)] undefNode
  in CDeclExt decl

attributeQual :: CDeclSpec
attributeQual =
  let n = Ident "__noreturn__" 0 undefNode
  in CTypeQual (CAttrQual (CAttr n [] undefNode))

externType :: CDeclSpec
externType = CStorageSpec (CExtern undefNode)

voidType :: CDeclSpec
voidType = CTypeSpec (CVoidType undefNode)

noReturn :: CDeclSpec
noReturn = CFunSpec (CNoreturnQual undefNode)

opts :: ParserInfo Configuration
opts = info ( config <**> helper )
  (  fullDesc
  <> header   "overdiagnose -- Given some conjunctive formula F (embedded in a CSC), \
              \ consider its subformulae, and output the (Overapproximator, Subformula) \
              \ pairs that cause some overapproximation." )

data Configuration = Configuration FilePath

config :: Parser Configuration
config = Configuration
  <$> argument str
      (  metavar "CSC_FILE" )

readCsc :: FilePath -> IO Csc
readCsc cscfile = do
  cscStr <- readFile cscfile
  let initCsc = read cscStr :: Csc
  return initCsc

lastAddedConjunction :: Csc -> Conjunction
lastAddedConjunction csc = upper $ upperBound $ last $ disjointPartitions csc

makeQuadruples :: InputRead -> [CExtDecl]
makeQuadruples (InputRead idNum typeSpec readNum) =
  let id = show idNum
      var = acaVarPrefix ++ (typeInfix typeSpec) ++ id
      numVar = acaNumPrefix ++ id
      counterVar = acaCounterPrefix ++ id
      varArr = acaArrPrefix ++ (typeInfix typeSpec) ++ id ++ "[" ++ (show readNum) ++ "]"
      decls = [
        (declareVar typeSpec var),
        (declareVarInit intType numVar "0"),
        (declareVarInit intType counterVar (show readNum)),
        (declareVar typeSpec varArr)
        ]
  in map CDeclExt decls
