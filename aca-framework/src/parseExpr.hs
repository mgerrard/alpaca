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

import Language.C.Analysis
--import Language.C.Test.Environment
--import Language.C.Test.GenericAST

import System.Environment (getEnv, getArgs)
import System.Exit
import Data.Generics
import Text.PrettyPrint.HughesPJ

main :: IO ()
main = do
  expr <- prompt "gimme a C expression: "

  _ <- parseMyExpr expr
  
  putStrLn ""  
  putStrLn "************************************************"
  putStrLn ""  
  main

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

parseMyExpr :: String -> IO CExpr
parseMyExpr str = runP expressionP str
  

runP :: P CExpr -> String -> IO CExpr
runP parser str =
  do
    expr <- either bailOut return $ pResult
    print (pretty expr)
    return expr
  where
  is = inputStreamFromString str
  pResult = execParser_ parser is (argPos)
  argPos = initPos "<cmd-line-arg>"

bailOut :: (Show err) => err -> IO a
bailOut err = do
    hPutStrLn stderr (show err)
    hPutStrLn stderr "*** Exit on Error ***"
    exitWith (ExitFailure 1)
