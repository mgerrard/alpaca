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
  hSetBuffering stdout NoBuffering
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch (killThread tid)) Nothing
  _ <- runAca =<< customExecParser (prefs (columns 120)) opts
  exitImmediately ExitSuccess

opts :: ParserInfo Configuration
opts = info ( versionOption <*> configuration <**> helper )
  (  fullDesc
  <> header   "ALPACA -- A Large Portfolio-based Alternating Conditional Analysis" )

versionOption :: Parser (a -> a)
versionOption = infoOption "ALPACA 1.1" (short 'v' <> long "version" <> help "show version")

configuration :: Parser Configuration
configuration = Configuration
  <$> argument str
      (  metavar "FILE" )
  <*> strOption
      (  long "debug"
      <> short 'd'
      <> showDefault
      <> metavar "LEVEL"
      <> value "quiet"
      <> help "(full | slice | analyzers | direct | generalize | quiet)" )
  <*> option auto
      (  long "timeout"
      <> short 't'
      <> help "global t.o. (in s) after first iteration"
      <> showDefault
      <> value 900
      <> metavar "INT" )
  <*> strOption
      (  long "portfolio"
      <> short 'p'
      <> showDefault
      <> metavar "FILTER"
      <> value "all"
      <> help "(all | allDock | cpaSeq | uAutomizer | veriAbs | pesco | symbiotic | esbmc | twoLs | cbmc | uTaipan | uKojak | pinaka | cpaBamSmg | cpaBamBnB | cvtAlgoSel | cvtParPort | goblint | graves | infer | lart | smack | theta | veriFuzz | (comma-sep. string of tools))")
  <*> option auto
      (  long "generalize-timeout"
      <> help "global t.o. (in s) for generalization phase"
      <> showDefault
      <> value 300
      <> metavar "INT" )
  <*> switch
      (  long "block-valid-paths"
      <> help "block spurious error paths" )
  <*> strOption
      (  long "exit-strategy"
      <> showDefault
      <> value "eager"
      <> metavar "ST"
      <> help "choose from (eager | patient)" )
  <*> strOption
      (  long "gen-exit-strategy"
      <> showDefault
      <> value "eager"
      <> metavar "ST"
      <> help "when generalizing, stopping strategy is (eager | patient)" )
  <*> strOption
      (  long "prefix"
      <> showDefault
      <> metavar "PATH"
      <> value "."
      <> help "prefix to the 'logs_aca/' directory" )
  <*> strOption
      (  long "target-function"
      <> showDefault
      <> metavar "F"
      <> value "__VERIFIER_error"
      <> help "function name to characterize reachability of" )
  <*> option auto
      (  long "partition-bound"
      <> help "number of maximum partitions before widening."
      <> showDefault
      <> value 4
      <> metavar "INT" )
  <*> option auto
      (  long "merge-length"
      <> help "number of partitions to merge when widening"
      <> showDefault
      <> value 2
      <> metavar "INT" )
  <*> strOption
      (  long "generalize-strategy"
      <> showDefault
      <> value "pessimisticEq"
      <> metavar "ST"
      <> help "(pessimisticEq | pessimisticDisEq | optimisticEq | optimisticDisEq)" )
  <*> strOption
      (  long "cpp-flags"
      <> value ""
      <> help "flags to pass to the C preprocessor")
  <*> option auto
      (  long "init-timeout"
      <> help "global t.o (in s) for initial iteration"
      <> showDefault
      <> value 900
      <> metavar "INT" )
  <*> strOption
      (  long "exclude"
      <> short 'e'
      <> showDefault
      <> metavar "FILTER"
      <> value ""
      <> help "selectively exclude analyzers")
  <*> strOption
      (  long "dse"
      <> showDefault
      <> metavar "TOOL"
      <> value "civl"
      <> help "who runs directed sym-exec to collect condition (civl | cpa)")
  <*> switch
      (  long "make-cud"
      <> help "Produce a .cud file for later digestion after first iteration" )
  <*> strOption
      (  long "chew-cud"
      <> showDefault
      <> metavar "CUD_FILE"
      <> value ""
      <> help "Pass in .cud file to see if ALPACA can chew on a subspace." )
  <*> switch
      (  long "minus-aca"
      <> help "Run portfolio once on uninstrumented file" )
  <*> strOption
      (  long "property"
      <> showDefault
      <> metavar "PRP"
      <> value "reachSafety"
      <> help "(reachSafety | memSafety | overflow)" )
  <*> switch
      (  long "known-reach"
      <> help "Counterfeit Detector when program is known to have at least one reach condition." )
  <*> switch
      (  long "baseline"
      <> help "Exit after single iteration." )
\end{code}