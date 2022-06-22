\begin{code}
module Main where

import Lib

import Options.Applicative
import Data.Semigroup ((<>))
import System.Exit
import System.Posix.Signals
import System.Posix.Process
import Control.Concurrent
import System.IO

main :: IO ()
main = do
  putStrLn "usage: runtools TOOL TOOL_DIR OUT_DIR C_FILE"

\end{code}