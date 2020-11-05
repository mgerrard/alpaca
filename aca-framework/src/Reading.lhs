\begin{code}
module Reading where

import Language.C
import System.Exit
import System.IO

parseToExpr :: String -> IO CExpr
parseToExpr str = runP expressionP str

runP :: P CExpr -> String -> IO CExpr
runP parser str =
  do
    expr <- either bailOut return $ pResult
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
\end{code}
