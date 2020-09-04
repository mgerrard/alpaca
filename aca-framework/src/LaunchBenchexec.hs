module LaunchBenchexec where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Portfolio

-- Unused tools from SVCOMP 18:
--  | Forester ; only for memsafety (and heap properties)
--  | PredatorHP ; only for memsafety (and heap properties)
--  | AProVE ; only for termination
--  | Yogar_CBMC ; only for concurrency
--  | Map2Check ; does not perform well
--  | Skink ; does not perform well
--  | VIAP ; does not perform well

data AnalysisResult = FalseResult | TrueResult | UnknownResult deriving (Show, Eq)

getResultSummary :: String -> AnalysisResult
getResultSummary output =
  let falseFound = isInfixOf " false(unreach-call) "
      trueFound = isInfixOf " true "
  in
    if falseFound output
      then FalseResult
      else if trueFound output
        then TrueResult
        else UnknownResult

{- TODO:
   - reducer-based incorporated into ACA
     - we describe failing subspaces
     - THEY describe non-failing subspaces
       - include this in some other partition
       - so we can start including this as another
         part of our disjoint partition
-}

xmlHeader :: Analyzer -> String
xmlHeader analyzer =
  "<?xml version=\"1.0\"?>\n\
  \<!DOCTYPE benchmark PUBLIC \"+//IDN sosy-lab.org//DTD BenchExec benchmark 1.4//EN\" \"http://www.sosy-lab.org/benchexec/benchmark-1.4.dtd\">\n\
  \<benchmark tool=\""++(analysisName analyzer)++"\" timelimit=\"900 s\" hardtimelimit=\"960 s\">\n\
  \<require cpuModel=\"Intel Xeon E3-1230 v5 @ 3.40 GHz\"/>\n\
  \<resultfiles>*</resultfiles>\n\
  \<rundefinition name=\"sv-comp19_prop-reachsafety\"></rundefinition>\n"

makeOption :: (Attribute, Maybe Value) -> String
makeOption (attr, Nothing) = "<option name=\""++attr++"\"/>\n"
makeOption (attr, Just value) = "<option name=\""++attr++"\">"++value++"</option>\n"

makeOptions :: Analyzer -> String
makeOptions tool = concat $ map makeOption (analysisOptions tool)

xmlFooter :: String
xmlFooter = "</benchmark>"

taskToRun :: FilePath -> FilePath -> String
taskToRun file aDir =
  let fileName = last $ splitOn "/" file
      propertyFile = aDir ++ "/PropertyUnreachCall.prp"
  in
    "<tasks name=\""++fileName++"\">\n\
    \<include>"++file++"</include>\n\
    \<propertyfile>"++propertyFile++"</propertyfile>\n\
    \</tasks>\n"

constructXML :: Analyzer -> FilePath -> FilePath -> String
constructXML a f d = (xmlHeader a) ++ (makeOptions a) ++ (taskToRun f d) ++ xmlFooter

