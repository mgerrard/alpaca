module Main where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  runTool =<< execParser opts
  return ()

opts :: ParserInfo RunToolConfiguration
opts = info ( configuration <**> helper )
  (  fullDesc
  <> header   "runtool -- test out a single portfolio tool on a C file" )

configuration :: Parser RunToolConfiguration
configuration = RunToolConfiguration
  <$> argument str
      (  metavar "TOOL" )
  <*> argument str
      (  metavar "FILE" )
  <*> option auto
      (  long "timeout"
      <> short 't'
      <> help "Set timeout (in seconds) for analysis tool"
      <> showDefault
      <> value 900
      <> metavar "INT" )

runTool :: RunToolConfiguration -> IO ()
runTool (RunToolConfiguration tool cFile t) = do
  setLibraryEnvironmentVariable
  as <- portfolio tool "" t t t
  let a = head as
  let outDir = "./runtoolLogs/"++cFile
  createDirectoryIfMissing True outDir
  {- The final three parameters are irrelevant;
     change the 4th if you want a different output dir -}
  (exitCode, stdOut, stdErr) <- runBenchexec cFile a t outDir Nothing True "."
  putStrLn ""
  putStrLn $ "Exit code: "++(show exitCode)++"\n"
  putStrLn $ "Stdout : "++(stdOut)++"\n"
  putStrLn $ "Stderr : "++(stdErr)++"\n"
  return ()

data RunToolConfiguration = RunToolConfiguration
  { runToolFile :: String
  , runToolTool :: String
  , runToolTime :: Int
  }
