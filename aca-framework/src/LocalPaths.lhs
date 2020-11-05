\begin{code}
module LocalPaths where

import System.Environment
import Control.Exception
import Data.Maybe (isJust)

analyzerDir :: IO String
analyzerDir = do
  dir <- lookupEnv "ACA_LIB"
  if isJust dir
    then do
      let (Just dirPath) = dir
      return dirPath
    else do
      putStrLn "Could not find path associated with environment variable $ACA_LIB. Aborting"
      assert False (return "")
\end{code}
