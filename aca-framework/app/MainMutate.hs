module Main where

import Data.Maybe
import Control.Monad.State
import System.Environment
import Language.C hiding (execParser) -- simple API
import Language.C.System.GCC   -- preprocessor used
import Language.C.Data.Ident
import Prelude hiding (lookup)

type AST = CTranslUnit
type Target = Int
type Loc = Int
type Ctx a = State (Target,Loc) a

main :: IO ()
main = do
  args <- getArgs
  let program = head args
  let pName = reverse $ (drop 2) $ reverse program 
  ast <- getAst program
  let mutants = makeMutants ast
  let mutIdxs = zip [0..] mutants
  mapM_ (\(idx, a) -> writeFile (pName++".mutant-"++(show idx)++".c") (show $ pretty a)) mutIdxs
  return ()

makeMutants :: AST -> [AST]
makeMutants ast = evalState (mutateAst ast []) (0,0)

mutateAst :: AST -> [AST] -> Ctx [AST]
mutateAst a as = do
  (targetIdx, _) <- get
  (a', finalIdx) <- mutateWorker a
  if (targetIdx < finalIdx) 
    then do
      put ((targetIdx+1),0)
      mutateAst a ([a']++as)
    else return as {- fixed point reached -}

mutateWorker :: AST -> Ctx (AST, Int)
mutateWorker (CTranslUnit es n) = do
  es' <- mapM processExtDecl es
  (_, finalLoc) <- get
  let ast' = CTranslUnit es' n
  return (ast', finalLoc)

processExtDecl :: CExtDecl -> Ctx CExtDecl
processExtDecl d@(CFDefExt _) = processFunction d
processExtDecl d = return d

processFunction :: CExtDecl -> Ctx CExtDecl
processFunction (CFDefExt (CFunDef dspec declr decls stmt a)) = do
  stmt' <- processStatement stmt
  return (CFDefExt (CFunDef dspec declr decls stmt' a))
processFunction _ = error "processFunction was not given a function"

processBlockItem :: CBlockItem -> Ctx (Maybe CBlockItem)
processBlockItem (CBlockStmt s) = do
  s' <- processStatement s
  return (Just (CBlockStmt s'))
processBlockItem _ = return Nothing

incrementLoc :: Ctx ()
incrementLoc = do { (target,loc) <- get; put (target,(loc+1)); return () }

verifierErrorCall :: CExpr
verifierErrorCall = (CCall (makeVar "__VERIFIER_error") [] undefNode)

makeVar :: String -> CExpr
makeVar s =
  let iden = makeIdent s
  in CVar iden undefNode

makeIdent :: String -> Ident
makeIdent s = Ident s 0 undefNode

errorStatement :: CStat
errorStatement = CExpr (Just verifierErrorCall) undefNode

updateNode :: Ctx (Maybe CStat)
updateNode = do
  (target, currIdx) <- get
  if (target == currIdx)
    then do
      put (target, (currIdx+1))
      return (Just errorStatement)
    else do
      put (target, (currIdx+1))
      return Nothing
  
processStatement :: CStat -> Ctx CStat
processStatement e@(CExpr _ _) = return e
processStatement (CLabel p1 s p3 p4) = do
  s' <- processStatement s
  return $ CLabel p1 s' p3 p4
processStatement (CCase p1 s p3) = do
  s' <- updateAndProcess s
  return $ CCase p1 s' p3
processStatement (CCases p1 p2 s p4) = do
  s' <- processStatement s
  return $ CCases p1 p2 s' p4
processStatement (CDefault s p2) = do
  s' <- updateAndProcess s
  return $ CDefault s' p2
processStatement (CCompound p1 bs p3) = do
  mBs' <- mapM processBlockItem bs
  let bs' = catMaybes mBs'
  return (CCompound p1 bs' p3)
processStatement (CIf p1 thenExpr Nothing p4) = do
  thenExpr' <- updateAndProcess thenExpr
  return $ CIf p1 thenExpr' Nothing p4
processStatement (CIf p1 s1 (Just s2) p4) = do
  s1' <- updateAndProcess s1
  s2' <- updateAndProcess s2
  return $ CIf p1 s1' (Just s2') p4
processStatement (CSwitch p1 s p3) = do
  s' <- processStatement s
  return $ CSwitch p1 s' p3
processStatement (CWhile p1 s p3 p4) = do
  s' <- updateAndProcess s
  return $ CWhile p1 s' p3 p4
processStatement (CFor p1 p2 p3 s p5) = do
  s' <- updateAndProcess s
  return $ CFor p1 p2 p3 s' p5
processStatement s = return s

updateAndProcess :: CStat -> Ctx CStat
updateAndProcess s1 = do
  maybeS <- updateNode
  s1' <- processStatement s1
  if (isJust maybeS)
    then do
      let (Just s0) = maybeS
      let block = CCompound [] [(CBlockStmt s0), (CBlockStmt s1')] undefNode
      return block
    else return s1'

getAst :: FilePath -> IO CTranslUnit
getAst p = parseCFile (newGCC "gcc") Nothing [] p >>= checkResult "[parsing]"

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return
