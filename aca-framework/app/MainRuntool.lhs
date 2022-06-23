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
    [tool,outDir,cFile] -> do
      putStrLn $ "args: "++tool++" "++outDir++" "++cFile
      createDirectoryIfMissing True outDir
      runtool tool outDir cFile
      return ()
    _ -> do
      putStrLn "usage: runtool TOOL OUT_DIR C_FILE_DIR"

runtool :: String -> FilePath -> FilePath -> IO (ToolResult, Maybe FilePath)
runtool "cpa" outDir fDir = do
  (_,stdOut,stdErr) <- readProcessWithExitCode "/usr/bin/docker" ["run","-v",outDir++":/alpaca_out","-v",fDir++":/alpaca_in","cpa"] ""
--  (_,stdOut,_) <- readProcessWithExitCode (toolDir++"/scripts/cpa.sh") ["-svcomp22", "-timelimit", "900s", f] ""
  putStrLn stdOut
  putStrLn stdErr
  let result = determineResult "cpa" stdOut
  putStrLn $ "cpa result: "++(show result)
  maybeWitness <- cpaWitness outDir
  if isJust maybeWitness
    then do
      let (Just w) = maybeWitness
      copyFile w (outDir++"/witness.graphml")
      putStrLn $ "witness at: "++outDir++"/witness.graphml"
      return (result,maybeWitness)
    else return (result,Nothing)
{-    
runtool "ua" outDir f = do
  (_,stdOut,stdErr) <- readProcessWithExitCode (toolDir++"/Ultimate.py") ["--full-output","--spec", 
    "/home/mjg6v/work/alpaca/tools/analyzer-portfolio/PropertyUnreachCall.prp", 
    "--file", f, "--architecture", "32bit", "--witness-dir", outDir] ""
  let result = determineResult "ua" stdOut
  putStrLn stdOut; putStrLn stdErr
  putStrLn $ "ua result: "++(show result)
  maybeWitness <- uaWitness outDir
  if isJust maybeWitness
    then do
      let (Just w) = maybeWitness
      copyFile w (outDir++"/witness.graphml")
      putStrLn $ "witness at: "++outDir++"/witness.graphml"
      return (result,maybeWitness)
    else return (result,Nothing)
-}    
runtool t _ _ = error $ "oops, i don't know the tool '"++t++"'"

determineResult :: String -> String -> ToolResult
determineResult "cpa" o =
  if isInfixOf "Verification result: FALSE" o
    then FalseResult
    else
      if isInfixOf "Verification result: TRUE" o
        then TrueResult
	else UnknownResult
determineResult "ua" o =
  if isInfixOf "Result:\nFALSE" o
    then FalseResult
    else
      if isInfixOf "Result:\nFALSE" o
        then TrueResult
	else UnknownResult
determineResult t _ = error $ "oops, i don't know how to parse the result for "++t

cpaWitness :: FilePath -> IO (Maybe FilePath)
cpaWitness outDir = do
  let witnessLoc = outDir++"/witness.graphml"
  witnessExists <- doesFileExist witnessLoc
  if witnessExists
    then return (Just witnessLoc)
    else return Nothing

uaWitness :: FilePath -> IO (Maybe FilePath)
uaWitness d = do
  let witnessLoc = d++"/witness.graphml"
  witnessExists <- doesFileExist witnessLoc
  if witnessExists
    then return (Just witnessLoc)
    else return Nothing

\end{code}