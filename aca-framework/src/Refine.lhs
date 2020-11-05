\begin{code}
{-# LANGUAGE MultiWayIf #-}

module Refine where

import CscTypes
import AcaComputation
import Configuration
import Control.Exception (SomeException, Exception, handle, assert)
import Data.List (nub, maximumBy)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.Maybe (isJust)
import Debug.Trace
import Network
import System.IO

runCount :: CountConfiguration -> IO ()
runCount (CountConfiguration cscHandle) = do
  cscStr <- readFile cscHandle
  let csc = read cscStr :: Csc
  countCsc csc
  return ()

countCsc :: Csc -> IO ()
countCsc csc = do
  --startCountingTime
  putStrLn "--- CSC counting call ---"
  putStrLn (show csc)
  let partitions = (disjointPartitions csc)
  putStrLn $ "** counting " ++ (show $ length partitions) ++ " CSCs **"
  counts <- mapM countPartition partitions
  putStrLn (show counts)
  let total = foldl (\ (x1,x2) (y1,y2) -> (x1 + y1,x2 + y2)) (0,0) counts
  putStrLn $ "[aca:refine] interval: " ++ (show total)
  putStrLn "-------------------------"
  --endCountingTime

countPartition :: CscPartition -> IO (RegionProbability)
countPartition partition = do
  let up = upper $ upperBound partition
  let lo = lowerBound partition
  putStrLn $ show up
  upResult <- countWithPCP up
  loResults <- mapM countWithPCP lo
  let upEstimate = getEstimate upResult
  let lowEstimates = map getEstimate loResults
  let lowSum = sum lowEstimates
  return (lowSum, upEstimate)

getEstimate :: CounterResult -> Rational
getEstimate (ExactResult estimate) = estimate
getEstimate (StatisticalResult estimate _) = estimate
  
prepareQuery :: Conjunction -> Text
prepareQuery conjunction =
  let header = declareDomain conjunction
      assertions = map (\clause -> Text.pack $ "(assert " <> (z3Conjunct clause) <> ")" ) (conjuncts conjunction)
--     concatenedAssertions = foldl (\x y -> x <> "\n" <> y ) "" assertions
      footer = ["(count)\n"]
  in Text.intercalate "\n" ([Text.pack "(clear)\n"] ++ header ++ (assertions) ++ footer)

declareDomain :: Conjunction -> [Text]
declareDomain conjunction =
  let vars = nub $ concat $ fmap inputVars (conjuncts conjunction)
  in fmap declareVar vars

declareVar :: InputVariable -> Text
declareVar (name, ctype) =
  if | ctype == "int" -> Text.pack $ "(declare-var " <> name <> "(Int -2147483647 2147483647))"
     | otherwise -> error "unsupported type"

-- TODO replace hardcoded constants
countWithPCP :: Conjunction -> IO CounterResult
countWithPCP conjunction = do
--  counter <- getCounter
--  let countCache  = cache counter
--  let cached = Map.lookup conjunction countCache
  let cached = Nothing
  if isJust cached
    then do
      let (Just val) = cached
      return val
    else do
      let pcpQuery = prepareQuery conjunction
      let pcpPort = 9001  -- port counter
      pcpResult <- callPCPSafe pcpQuery pcpPort
      if isJust pcpResult
        then do
          let (Just count) = pcpResult
--          let updatedMap = Map.insert conjunction count countCache
--          st <- get
--        put $ st { stateCounter = (Counter updatedMap pcpPort) }
          return count
        else do -- TODO change this to either?
          assert False (return (ExactResult 42.0))

callPCPSafe :: Text -> Int -> IO (Maybe CounterResult)
callPCPSafe query p = handle errorHandler (callPCP query p)
  where errorHandler :: SomeException -> IO (Maybe CounterResult)
        errorHandler _ = do
          putStrLn "[aca:refine] Error connecting to PCP!"
          return Nothing

-- TODO remove localhost and add a real address to the config
callPCP :: Text -> Int -> IO (Maybe CounterResult)
callPCP query p = withSocketsDo $ do
     handle <- connectTo "localhost" (PortNumber (fromInteger (toInteger p)))
     hSetBuffering handle LineBuffering
     hPutStrLn handle $ Text.unpack query
     reply <- hGetLine handle
     hClose handle
     putStrLn reply
     case parsePCPReply (Text.pack reply) of
       Left e -> do
         print $ "Malformed pcp output: " ++ e
         return Nothing
       Right cr -> do
         return $ Just cr

parsePCPReply :: Text -> Either String CounterResult
parsePCPReply s =
  let sLen = Text.length s 
      noPar = Text.drop 1 $ Text.take (sLen - 1) s
      chunks = filter (not . Text.null) $ Text.splitOn " " noPar
  in case chunks of
    [] -> Left "${empty message}"
    (x:xs)  -> checkReplyType x xs

-- TODO !! should by replaced by Lens
checkReplyType :: Text -> [Text] -> Either String CounterResult
checkReplyType a b | trace ((show a) ++ " " ++ (show b)) False = undefined
checkReplyType mtype chunks
  | Text.isPrefixOf "exact" mtype =
    let fractionStr = (Text.unpack $ Text.replace "/" " % " $ head chunks)
    in Right $ ExactResult (read fractionStr :: Rational)
  | Text.isPrefixOf "statistical" mtype =
    let estimate = (read (Text.unpack $ Text.replace "/" " % " $ head chunks) :: Rational)
        variance = (read (Text.unpack $ Text.replace "/" " % " $ chunks !! 2) :: Rational)
    in Right $ StatisticalResult estimate variance
  | Text.isPrefixOf "error" mtype = Left $ "pcp error: " ++ (Text.unpack mtype) ++ (show chunks)
  | otherwise = Left $ "unknown error: " ++ (Text.unpack mtype) ++ (show chunks)

data GapMeasure = ArbitraryMeasure | SolutionCount
data RefineStrategy = ArbitraryStrategy

-- min/max domain percentage covered by a partition
type RegionProbability = (Rational,Rational)
\end{code}
