module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory
import Data.List (sortBy)
import Text.XHtml.Strict (stringToHtmlString)
import Data.String.Utils (replace)
import Data.Maybe (catMaybes)
import Control.Exception (evaluate)
import Control.DeepSeq (force)

main :: IO ()
main = do
  runCollect =<< execParser opts
  return ()

opts :: ParserInfo Configuration
opts = info ( config <**> helper )
  (  fullDesc
  <> header   "CollectResults -- Generate an HTML page summarizing runs of ALPACA" )

data Configuration = Configuration { logDirectory :: FilePath }

config :: Parser Configuration
config = Configuration
  <$> argument str
      (  metavar "LOG_DIR" )

{-
This program assumes the invariants:
   + the logDir parameter is a valid directory that contains only
     directories named after the files on which ALPACA was run
   + each of the subdirectories contains a file named "exit.summary",
     which contains up to three lines, the first is the file name,
     the second the exit status (exact, gap, top, or error), and
     the third is a summary message (either the runtime in seconds, or
     where the error occurred); if only the first line exists, the
     file is considered "non-finishing"
   + each of the subdirectories may optionally contain files with the:
     - CSC (placed in "final.csc")
     - stdout (placed in "stdout")
     - stderr (placed in "stderr")
     - Portfolio log (placed in "portfolio.log")
     - Runtime statistics (placed in "statistics")
-}

runCollect :: Configuration -> IO ()
runCollect (Configuration logDir) = do
  htmlPage <- generatePage logDir
  absoluteHtmlPage <- makeAbsolute htmlPage
  putStrLn $ "Results summary printed to "++absoluteHtmlPage
  return ()

generatePage :: FilePath -> IO FilePath
generatePage logDir = do
  fileDirs <- listDirectory logDir
  maybeSummaries <- mapM (collectSummary logDir) fileDirs
  let summaries = catMaybes maybeSummaries
  let exacts = sortByName $ filter (\y -> (category y)==Exact) summaries
  let gaps = sortByName $ filter (\y -> (category y)==Gap) summaries
  let tops = sortByName $ filter (\y -> (category y)==Top) summaries
  let errors = sortByName $ filter (\y -> (category y)==Error) summaries
  let nonfinishers = sortByName $ filter (\y -> (category y)==Nonfinishing) summaries
  let metasummary = collectMetaSummary exacts gaps tops errors nonfinishers
  let summarySection = summaryHtml metasummary
  let exactSection = sectionHtml exacts "Exact"
  let gapSection = sectionHtml gaps "Gap"
  let topSection = sectionHtml tops "Top"
  let errorSection = sectionHtml errors "Error"
  let nonfinishSection = sectionHtml nonfinishers "Non-finishing"
  let page = [htmlHeader,
              summarySection,
              exactSection,
              gapSection,
              topSection,
              errorSection,
              nonfinishSection,
              htmlFooter]
  let htmlString = concat page
  let htmlHandle = "./alpaca-results.html"
  writeFile htmlHandle htmlString
  return htmlHandle

sortByName :: [RunSummary] -> [RunSummary]
sortByName rs = (sortBy (\a b -> compare (fileName a) (fileName b))) rs

sectionHtml :: [RunSummary] -> String -> String
sectionHtml ss sectionName =
  "<button class=\"accordion\">"++(sectionName)++"</button>\n\
  \<div class=\"panel\">"++(concat $ map fileHtml ss)++"\n\
  \</div>\n\
\"

fileHtml :: RunSummary -> String
fileHtml s =
  "<button class=\"inner-accordion\">"++(fileName s)++"<span style=\"float:right;\">"++(formatMessage s)++"</span></button>\n\
\<div class=\"panel\">\n\
\"++(fileSubcategoryHtml (csc s) "CSC")++"\n\
\"++(fileSubcategoryHtml (configuration s) "Configuration")++"\n\
\"++(fileSubcategoryHtml (stdout s) "stdout")++"\n\
\"++(fileSubcategoryHtml (stderr s) "stderr")++"\n\
\"++(fileSubcategoryHtml (portfolioLog s) "Portfolio log")++"\n\
\"++(fileSubcategoryHtml (statistics s) "Statistics")++"\n\
\</div>\n\
\"

formatMessage :: RunSummary -> String
formatMessage (RunSummary _ Error m _ _ _ _ _ _) = m
formatMessage s = "("++(rhsMessage s)++" s)"

data RunSummary = RunSummary {
  fileName :: FilePath,
  category :: ExitCategory,
  rhsMessage :: String,
  csc :: Maybe String,
  configuration :: Maybe String,
  stdout :: Maybe String,
  stderr :: Maybe String,
  portfolioLog :: Maybe String,
  statistics :: Maybe String
} deriving (Show)

fileSubcategoryHtml :: Maybe String -> String -> String
fileSubcategoryHtml Nothing _ = ""
fileSubcategoryHtml (Just s) subcategoryName =
  "<button class=\"inner-accordion\">"++subcategoryName++"</button>\n\
  \<div class=\"panel\"><p>"++s++"</p></div>"

collectMetaSummary :: [a] -> [a] -> [a] -> [a] -> [a] -> MetaSummary
collectMetaSummary ex ga to er nf =
  let exNum = length ex
      gaNum = length ga
      toNum = length to
      erNum = length er
      nfNum = length nf
      total = exNum+gaNum+toNum+erNum+nfNum
  in (MetaSummary exNum gaNum toNum erNum nfNum total)

summaryHtml :: MetaSummary -> String
summaryHtml ms =
  "<h2>Summary of ALPACA run on "++(show $ totalNum ms)++" files</h2>\n\
  \<h4>&nbsp;&nbsp;Exact CSC: "++(show $ exactNum ms)++"</h4>\n\
  \<h4>&nbsp;&nbsp;CSC with non-trivial gap: "++(show $ gapNum ms)++"</h4>\n\
  \<h4>&nbsp;&nbsp;CSC with an upper bound of Top: "++(show $ topNum ms)++"</h4>\n\
  \<h4>&nbsp;&nbsp;Terminated with some error: "++(show $ errorNum ms)++"</h4>\n\
  \<h4>&nbsp;&nbsp;Non-finishing: "++(show $ nonfinishNum ms)++"</h4>\n"

data MetaSummary = MetaSummary {
  exactNum :: Int,
  gapNum :: Int,
  topNum :: Int,
  errorNum :: Int,
  nonfinishNum :: Int,
  totalNum :: Int
}

isExitFileValid :: FilePath -> IO Bool
isExitFileValid exitFile = do
  exitFileExists <- doesFileExist exitFile
  if exitFileExists
    then do
      f <- readFile exitFile
      _ <- evaluate $ force f
      let line_num = length (lines f)
      if (line_num < 1)
        then do
          putStrLn $ "The file "++exitFile++" is malformatted and will not be included in the summary."
          return False
        else return True
    else return False

collectSummary :: FilePath -> FilePath -> IO (Maybe RunSummary)
collectSummary logDir file = do
  let base = logDir++"/"++file
  let exitFile = base++"/exit.summary"
  exitFileExists <- isExitFileValid exitFile
  if exitFileExists
    then do
      (fName, cat, rhs) <- getExitSummary exitFile
      mCsc <- tryRead base "final.csc"
      mConfig <- tryRead base "config"
      mStdout <- tryRead base "stdout"
      mStderr <- tryRead base "stderr"
      mPortLog <- tryRead base "portfolio.log"
      mStats <- tryRead base "statistics"
      return $ (Just (RunSummary fName cat rhs mCsc mConfig mStdout mStderr mPortLog mStats))
    else return Nothing

getExitSummary :: FilePath -> IO (FilePath, ExitCategory, String)
getExitSummary exitFile = do
  let exitSummary = exitFile
  contents <- readFile exitSummary
  _ <- evaluate $ force contents
  let linesOfFile = lines contents
  let fName = linesOfFile !! 0
  if ((length linesOfFile) > 1)
    then do
      let cat = deriveCategory (linesOfFile !! 1)
      let rhs = linesOfFile !! 2
      return (fName, cat, rhs)
    else do
      return (fName, Nonfinishing, "")

tryRead :: FilePath -> FilePath -> IO (Maybe String)
tryRead base f = do
  let file = base++"/"++f
  fileExists <- doesFileExist file
  if fileExists
    then do
      fileStr <- readFile file
      _ <- evaluate $ force fileStr
      return (Just (stringToMyHtmlString fileStr))
    else return Nothing

stringToMyHtmlString :: String -> String
stringToMyHtmlString s =
  let s' = stringToHtmlString s
  in replace "\n" "<br>" s'

deriveCategory :: String -> ExitCategory
deriveCategory "exact" = Exact
deriveCategory "gap" = Gap
deriveCategory "top" = Top
deriveCategory "error" = Error
deriveCategory "nonfinishing" = Nonfinishing
deriveCategory c = error $ "Illegal category: "++c

data ExitCategory = Exact | Gap | Top | Error | Nonfinishing deriving (Eq, Show)

htmlHeader :: String
htmlHeader =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \<head>\n\
  \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
  \<style>\n\
  \.accordion {\n\
  \background-color: #CEE3F6;\n\
  \color: #444;\n\
  \cursor: pointer;\n\
  \padding: 12px;\n\
  \width: 100%;\n\
  \border: none;\n\
  \text-align: left;\n\
  \outline: none;\n\
  \font-size: 15px;\n\
  \transition: 0.4s;\n\
\}\n\
\.inner-accordion {\n\
\  background-color: #eee;\n\
\  color: #444;\n\
\  cursor: pointer;\n\
\  padding: 5px;\n\
\  width: 100%;\n\
\  border: none;\n\
\  text-align: left;\n\
\  outline: none;\n\
\  font-size: 15px;\n\
\  transition: 0.4s;\n\
\}\n\
\.out-active, .accordion:hover {\n\
\  background-color: #819FF7; \n\
\}\n\
\.in-active, .inner-accordion:hover {\n\
\  background-color: #ccc; \n\
\}\n\
\.panel {\n\
\  padding: 0 18px;\n\
\  display: none;\n\
\  background-color: white;\n\
\  overflow: hidden;\n\
\}\n\
\</style>\n\
\</head>\n\
\<body>\n\
\"

htmlFooter :: String
htmlFooter =
  "<script>\n\
\var acc = document.getElementsByClassName(\"accordion\");\n\
\var i;\n\
\for (i = 0; i < acc.length; i++) {\n\
\  acc[i].addEventListener(\"click\", function() {\n\
\    this.classList.toggle(\"out-active\");\n\
\    var panel = this.nextElementSibling;\n\
\    if (panel.style.display === \"block\") {\n\
\      panel.style.display = \"none\";\n\
\    } else {\n\
\      panel.style.display = \"block\";\n\
\    }\n\
\  });\n\
\}\n\
\\n\
\acc = document.getElementsByClassName(\"inner-accordion\");\n\
\\n\
\for (i = 0; i < acc.length; i++) {\n\
\  acc[i].addEventListener(\"click\", function() {\n\
\    this.classList.toggle(\"in-active\");\n\
\    var panel = this.nextElementSibling;\n\
\    if (panel.style.display === \"block\") {\n\
\      panel.style.display = \"none\";\n\
\    } else {\n\
\      panel.style.display = \"block\";\n\
\    }\n\
\  });\n\
\}\n\
\\n\
\</script>\n\
\\n\
\</body>\n\
\</html>\n\
\"
