import System.Environment (getArgs)
import System.Directory (doesFileExist)

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

import Language.C

import Reading (parseToExpr)
import Data.Char (isSpace)

import System.Process (readProcessWithExitCode)
import Data.Ratio
import Numeric
import Data.Time

import SolverLib
import Data.SBV

import qualified Data.Map.Strict as Map
import Data.List (findIndex, isPrefixOf, find, any, dropWhileEnd, isInfixOf, intercalate, nub)

import Data.List (elemIndex)
data Partition = Partition CExpr [CExpr]
  | TruePartition
  deriving (Show,Eq)

type Op = String
type Var = String
type LookupTable = Map.Map String (Op, Var, Var)

negatePrecedingPartitions :: [Partition] -> IO [Partition]
negatePrecedingPartitions ps = do
  let idPs = zip [0..] ps
  mapM (negateWorker ps) idPs

negateWorker :: [Partition] -> (Int,Partition) -> IO Partition
negateWorker _ (_,TruePartition) = return TruePartition
negateWorker ps (i, (Partition p lbs)) = do
  mNegations <- mapM (negateP $ p) (take i ps)
  let negations = catMaybes mNegations
      p' = makeConjunctionExpr ([p]++negations)
  return $ Partition p' lbs

overlaps :: CExpr -> CExpr -> IO Bool
overlaps a b = do
  let conj = makeConjunctionExpr [a,b]
  let env = nub $ map extractVarName $ extractCIndexes conj
  r <- isSat conj env
  return $ isSatRes' r

isSatRes' :: SatResult -> Bool
isSatRes' (SatResult (Satisfiable _ _)) = True
isSatRes' _ = False

negateP :: CExpr -> Partition -> IO (Maybe CExpr)
negateP e (Partition p _) = do
  return $ Just $ CUnary CNegOp p undefNode
negateP _ _ = error "Should not be negating a TruePartition"

makePartition :: String -> IO Partition
makePartition s = do
  let ls = filter (not . null) (lines s)
  if (length ls) < 3
    then return TruePartition
    else do
      let up = ls !! 2
          lo = ls !! 4
      if up /= lo
        then do
          upF <- parseToExpr (cleanUp up)
          let ls' = drop 4 ls
              ls'' = takeWhile (\l -> not $ "Assumptions:" `isInfixOf` l) ls'
          lowers <- mapM parseToExpr ls''
          return $ Partition upF lowers
        else do
          let up' = cleanUp up
          if null up'
            then return $ TruePartition
            else do
              formula <- parseToExpr up'
              return $ Partition formula [formula]

cleanUp :: String -> String
cleanUp s = trim $ head $ splitOn "MINUS" s

gapCountStr :: [Rational] -> String
gapCountStr rs =
  let rs' = filter (>0) rs
      rs'' = map display rs'
  in
    if null rs''
      then "no-gap"
      else intercalate ":" rs''

gatherGapCount :: PartitionCount -> Rational
gatherGapCount (PartitionCount (_,uCount) ls) =
  let lCounts = map (\(_,c)->c) ls
      lSum = sum lCounts
      gapCount = uCount - lSum
  in gapCount

displayPartitionCount :: PartitionCount -> IO ()
displayPartitionCount (PartitionCount uCount lCounts) = do
  putStrLn $ "\nPartition:"
  putStrLn $ "  Upper:"
  displayCount uCount
  putStrLn $ "  Lower:"
  mapM_ displayCount lCounts

displayCount :: (Int,Rational) -> IO ()
displayCount (t,c) = putStrLn $ "     time: "++(show t)++", count:"++(display c)

countPartitions :: [Partition] -> IO [PartitionCount]
countPartitions ps = do
  let hasTruePC = any (\p -> p == TruePartition) ps
  if hasTruePC
    then return $ [PartitionCount (0,1 % 1) []]
    else mapM countPartition ps

data PartitionCount = PartitionCount {
  upperCount :: (Int,Rational),
  lowerCount :: [(Int,Rational)]
}

countPartition :: Partition -> IO PartitionCount
countPartition p = do
  let upC = (0,0 % 1)
  lowC <- modelCountLow p
  return $ PartitionCount upC lowC
countPartition _ = error "should have already processed the TruePartitions"

modelCountUp :: Partition -> IO (Int,Rational)
modelCountUp TruePartition = return $ (0,1 % 1)
modelCountUp (Partition p _) = do
  r <- count p
  return r

modelCountLow :: Partition -> IO [(Int,Rational)]
modelCountLow TruePartition = return $ [(0,1 % 1)]
modelCountLow (Partition up lowers) = do
  let lowers' = map (\l->makeConjunctionExpr [up,l]) lowers
  rs <- mapM count lowers'
  return rs

pcpPath :: String
pcpPath =
  "/home/mitch/work/tools/PathConditionsProbability/startPCP"

count :: CExpr -> IO (Int,Rational)
count e = do
  if (show $ pretty e) == "1"
    then return (0,1 % 1)
    else do
      smtStr <- extractSMTQuery e
      let query = sanitizeSMT smtStr
          query' = addPcpHeader query
      writeFile "pcp.query" query'
      start <- getCurrentTime
      (_,out,_) <- readProcessWithExitCode pcpPath [] query
      end <- getCurrentTime
      let runtime = ceiling $ realToFrac $ diffUTCTime end start
      result <- parsePcpOut out
      return (runtime, result)

addPcpHeader :: String -> String
addPcpHeader s =
  "(set-option :non-linear-counter \"qcoral\")\n"++
  "(set-option :partitioning true)\n"++
  "(set-option :inclusion-exclusion false)\n"++
  "(set-option :linear-counter \"barvinok\")\n"++
  "(set-option :simplify \"z3\")\n"++s

display :: Rational -> String
display r = (show $ numerator r)++"/"++(show $ denominator r)

parsePcpOut :: String -> IO Rational
parsePcpOut o = do
  let ls = lines o
      (Just result) = find (isPrefixOf "(exact:") ls
  let f = init $ head $ tail $ words result
      [numer,denom] = splitOn "/" f
  return $ (read numer::Integer) % (read denom::Integer)

extractSMTQuery :: CExpr -> IO String
extractSMTQuery e = do
  generateSMTBenchmark True (makePredicate e)

sanitizeSMT :: String -> String
sanitizeSMT s =
  let ss = lines s
      (Just constantIdx) = elemIndex "; --- literal constants ---" ss
      (Just variableIdx) = elemIndex "; --- skolem constants ---" ss
      (Just varEndIdx) = elemIndex "; --- constant tables ---" ss
      (Just formulaIdx) = elemIndex "; --- formula ---" ss
      formEndIdx = length ss

      constants = slice (constantIdx) (variableIdx-1) ss
      variables = slice (variableIdx) (varEndIdx-1) ss
      formula = slice (formulaIdx) (formEndIdx-1) ss

      cs = transformConstants constants
      vs = transformVariables variables
      fs = transformFormula formula
  in unlines (cs++vs++fs)

slice from to xs = take (to - from + 1) (drop from xs)

transformConstants :: [String] -> [String]
transformConstants ss = map toConstant ss

toConstant :: String -> String
toConstant "; --- literal constants ---" =
  "; --- literal constants ---"
toConstant s =
  let tokens = splitOn " " s
  in
    if ((length tokens) > 3)
      then 
        let varName = tokens !! 1
            ty = tokens !! 3
            suffix = last tokens
        in "(declare-const "++varName++" "++ty++" "++suffix
      else error "toConstant assumption violation"

transformVariables :: [String] -> [String]
transformVariables ss = map toVariable ss

toVariable :: String -> String
toVariable "; --- skolem constants ---" =
  "; --- skolem constants ---"
toVariable s =
  if (length (splitOn " " s)) > 1
    then
      let varName = (splitOn " " s) !! 1
      in "(declare-var "++varName++" (Int -1073741823 1073741823))"
    else error "toVariable assertion violation"

transformFormula :: [String] -> [String]
transformFormula ss =
  let table = foldr processLine Map.empty ss
      (Just assertIdx) = findIndex (\l-> "(assert " `isPrefixOf` l) ss
      aLine = ss !! assertIdx
      kernel = init $ (splitOn " " aLine) !! 1
      assertion = "(assert "++(expandExpr kernel table)++")"
  in ["; --- formula ---", assertion, "(count)"]

expandExpr :: String -> Map.Map String (Op, Var, Var) -> String
expandExpr expr m =
  case Map.lookup expr m of
    Nothing -> expr
    Just ("distinct", v1, v2) ->
      "(not (= "++(expandExpr v1 m)++" "++(expandExpr v2 m)++"))"
    Just (op, v1, "UNARY") ->
      "("++op++" "++(expandExpr v1 m)++")"    
    Just (op, v1, v2) ->
      "("++op++" "++(expandExpr v1 m)++" "++(expandExpr v2 m)++")"

processLine :: String -> LookupTable -> LookupTable
processLine s m =
  if (not ("(define-fun " `isPrefixOf` s))
    then m
    else Map.insert (grabKey s) (makeEntry s) m

grabKey :: String -> String
grabKey s =
  if (length (splitOn " " s)) > 0
    then
      let ss = splitOn " " s
      in (ss !! 1)
    else error "grabKey assertion violation"

makeEntry :: String -> (Op, Var, Var)
makeEntry s =
  if (length (splitOn " " s)) > 6
    then
      let ss = splitOn " " s
          op = tail $ ss !! 4
          v1 = ss !! 5
          v2 = ss !! 6
          v2' = takeWhile (/=')') v2
      in (op, v1, v2')
    else
      let ss = splitOn " " s
          op = tail $ ss !! 4
          v1 = ss !! 5
          v1' = takeWhile (/=')') v1
      in (op, v1', "UNARY")

trim = dropWhileEnd isSpace . dropWhile isSpace


main :: IO ()
main = do
  args <- getArgs
  if ((length args) /= 1)
    then error "Expecting one arg. Usage: count CSC_FILE"
    else return ()
    
  let cscFile = head args
  fileExists <- doesFileExist cscFile
  if (not fileExists)
    then error $ "The file "++cscFile++" does not exist."
    else return ()

  fileStr <- readFile cscFile
  let partitionStrs = splitOn "\n\n" fileStr
  partitions <- mapM makePartition partitionStrs
  noOverlaps <- negatePrecedingPartitions partitions

  results <- countPartitions noOverlaps
  let psiCount = sum $ map (\(PartitionCount _ ls)->sum $ map (\(_,r)->r) ls) results
  let lowerTimes = concat $ map (\(PartitionCount _ ls) -> map (\(t,_)->t) ls) results
  let maxTime = maximum lowerTimes

  -- output: #(psi), #-time
  putStrLn $ (display psiCount)++","++(show maxTime)

  return ()

