\begin{code}
module LaunchTool where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Portfolio

data AnalysisResult = FalseResult | TrueResult | UnknownResult deriving (Show, Eq)

falseFound :: String -> Bool
falseFound s = isInfixOf "esult: FALSE" s || isInfixOf "VERIABS_VERIFICATION_FAILED" s
--falseFound s = isInfixOf " false(unreach-call) " s || isInfixOf " false(valid-deref) " s || isInfixOf " false(valid-free) " s || isInfixOf " false(valid-memtrack) " s || isInfixOf " false(no-overflow) " s

-- temporary change, need to make an interface for each tool to parse output
getResultSummary :: String -> AnalysisResult
getResultSummary output =
  let trueFound = (isInfixOf "esult: TRUE" output) || (isInfixOf "VERIABS_VERIFICATION_SUCCESSFUL" output)
  in
    if falseFound output
      then FalseResult
      else if trueFound
        then TrueResult
        else UnknownResult

propFilePath :: FilePath -> Bool -> Property -> String
propFilePath aDir False prp = aDir++"/"++(propFile prp)
propFilePath _ True prp = "/"++(propFile prp)

propFile :: Property -> String
propFile ReachSafety = "PropertyUnreachCall.prp"
propFile MemSafety = "MemSafety.prp"
propFile OverflowSafety = "Overflow.prp"

\end{code}
