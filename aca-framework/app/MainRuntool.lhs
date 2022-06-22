\begin{code}
module Main where

import Lib

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (isJust)
import Data.List (isInfixOf)
import System.Exit
import System.Directory
import System.Environment
import System.Process
import System.Posix.Signals
import System.Posix.Process
import Control.Concurrent
import System.IO

data ToolResult = TrueResult | FalseResult | UnknownResult deriving (Show,Eq)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tool,toolDir,outDir,cFile] -> do
      putStrLn $ "args: "++tool++" "++toolDir++" "++outDir++" "++cFile
      createDirectoryIfMissing True outDir
      runtool tool toolDir outDir cFile
      return ()
    _ -> do
      putStrLn "usage: runtool TOOL TOOL_DIR OUT_DIR C_FILE"

runtool :: String -> FilePath -> FilePath -> FilePath -> IO (ToolResult, Maybe FilePath)
runtool "cpa" toolDir outDir f = do
  (_,stdOut,_) <- readProcessWithExitCode (toolDir++"/scripts/cpa.sh") ["-svcomp22", "-timelimit", "900s", f] ""
  let result = determineResult "cpa" stdOut
  putStrLn $ "cpa result: "++(show result)
  maybeWitness <- cpaWitness
  if isJust maybeWitness
    then do
      let (Just w) = maybeWitness
      copyFile w (outDir++"/witness.graphml")
      putStrLn $ "copied witness to: "++outDir++"/witness.graphml"
      return (result,maybeWitness)
    else return (result,Nothing)
runtool t _ _ _ = error $ "oops, i don't know the tool '"++t++"'"

determineResult :: String -> String -> ToolResult
determineResult "cpa" o =
  if isInfixOf "Verification result: FALSE" o
    then FalseResult
    else
      if isInfixOf "Verification result: TRUE" o
        then TrueResult
	else UnknownResult
     
determineResult t _ = error $ "oops, i don't know how to parse the result for "++t

cpaWitness :: IO (Maybe FilePath)
cpaWitness = do
  let witnessLoc = "output/witness.graphml"
  witnessExists <- doesFileExist witnessLoc
  if witnessExists
    then return (Just witnessLoc)
    else return Nothing

\end{code}