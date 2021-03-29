\begin{code}
module LaunchBenchexec where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Portfolio
import CscTypes (Property (..))

data AnalysisResult = FalseResult | TrueResult | UnknownResult deriving (Show, Eq)

falseFound :: String -> Bool
falseFound s = isInfixOf " false(unreach-call) " s || isInfixOf " false(valid-deref) " s || isInfixOf " false(valid-free) " s || isInfixOf " false(valid-memtrack) " s || isInfixOf " false(no-overflow) " s

getResultSummary :: String -> AnalysisResult
getResultSummary output =
  let trueFound = isInfixOf " true "
  in
    if falseFound output
      then FalseResult
      else if trueFound output
        then TrueResult
        else UnknownResult

xmlHeader :: Analyzer -> String
xmlHeader analyzer =
  "<?xml version=\"1.0\"?>\n\
  \<!DOCTYPE benchmark PUBLIC \"+//IDN sosy-lab.org//DTD BenchExec benchmark 1.4//EN\" \"http://www.sosy-lab.org/benchexec/benchmark-1.4.dtd\">\n\
  \<benchmark tool=\""++(analysisName analyzer)++"\" timelimit=\"900 s\" hardtimelimit=\"960 s\">\n\
  \<require cpuModel=\"Intel Xeon E3-1230 v5 @ 3.40 GHz\"/>\n\
  \<resultfiles>*</resultfiles>\n\
  \<rundefinition name=\"sv-comp20_prop-reachsafety\"></rundefinition>\n"

makeOption :: (Attribute, Maybe Value) -> String
makeOption (attr, Nothing) = "<option name=\""++attr++"\"/>\n"
makeOption (attr, Just value) = "<option name=\""++attr++"\">"++value++"</option>\n"

makeOptions :: Analyzer -> String
makeOptions tool = concat $ map makeOption (analysisOptions tool)

xmlFooter :: String
xmlFooter = "</benchmark>"

taskToRun :: FilePath -> FilePath -> Bool -> Property -> String
taskToRun file aDir dock prp =
  let fileName = last $ splitOn "/" file
      propertyFile = propFilePath aDir dock prp
  in
    "<tasks name=\""++fileName++"\">\n\
    \<include>"++file++"</include>\n\
    \<propertyfile>"++propertyFile++"</propertyfile>\n\
    \</tasks>\n"

propFilePath :: FilePath -> Bool -> Property -> String
propFilePath aDir False prp = aDir++"/"++(propFile prp)
propFilePath _ True prp = "/"++(propFile prp)

propFile :: Property -> String
propFile ReachSafety = "PropertyUnreachCall.prp"
propFile MemSafety = "MemSafety.prp"
propFile OverflowSafety = "Overflow.prp"

constructXML :: Analyzer -> FilePath -> FilePath -> Bool -> Property -> String
constructXML a f d dock prp = (xmlHeader a) ++ (makeOptions a) ++ (taskToRun f d dock prp) ++ xmlFooter

\end{code}
