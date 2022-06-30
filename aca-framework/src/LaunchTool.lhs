\begin{code}
module LaunchTool where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Portfolio

data AnalysisResult = FalseResult | TrueResult | UnknownResult deriving (Show, Eq)

falseFound :: String -> Analyzer -> Bool
falseFound s (Analyzer CBMC _ _ _ _ _ _ _ _) = isInfixOf "VERIFICATION FAILED" s
falseFound s (Analyzer ESBMC _ _ _ _ _ _ _ _) = isInfixOf "FALSE_REACH" s
falseFound s (Analyzer TwoLS _ _ _ _ _ _ _ _) = isInfixOf "VERIFICATION FAILED" s
falseFound s (Analyzer VeriAbs _ _ _ _ _ _ _ _) = isInfixOf "VERIABS_VERIFICATION_FAILED" s
falseFound s (Analyzer VeriFuzz _ _ _ _ _ _ _ _) = (isInfixOf "FALSE(unreach-call)" s) || (isInfixOf "VERIFUZZ_VERIFICATION_FAILED" s)
falseFound s _ = isInfixOf "esult: FALSE" s

trueFound :: String -> Analyzer -> Bool
trueFound s (Analyzer CBMC _ _ _ _ _ _ _ _) = isInfixOf "VERIFICATION SUCCESSFUL" s
trueFound s (Analyzer ESBMC _ _ _ _ _ _ _ _) = isInfixOf "TRUE" s
trueFound s (Analyzer TwoLS _ _ _ _ _ _ _ _) = isInfixOf "VERIFICATION SUCCESSFUL" s
trueFound s (Analyzer VeriAbs _ _ _ _ _ _ _ _) = isInfixOf "VERIABS_VERIFICATION_SUCCESSFUL" s
trueFound s (Analyzer VeriFuzz _ _ _ _ _ _ _ _) = isInfixOf "VERIFUZZ_VERIFICATION_SUCCESSFUL" s
trueFound s _ = isInfixOf "esult: TRUE" s

getResultSummary :: String -> Analyzer -> AnalysisResult
getResultSummary output a =
  if falseFound output a
    then FalseResult
    else if trueFound output a
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
