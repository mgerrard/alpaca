\begin{code}
import Parsing
import System.Environment
import System.Process
import System.IO.Silently
import Control.Arrow
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map

import Language.C              -- simple API
import Language.C.Analysis     -- analysis API
import Language.C.System.GCC   -- preprocessor used

import Data.String.Utils

identityTransform :: IO ()
identityTransform = do
  inputAst <- getAst "identity.input.c"
  let output  = ((show . pretty) inputAst) ++ "\n"
  oracle <- readFile "identity.input.c"
  writeFile "identity.output.c" output

main = do
  identityTransform
  callCommand "diff -w identity.output.c identity.oracle.c"
  putStrLn "*** All tests passed ***"
\end{code}
