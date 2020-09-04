module Statistics where

import Data.Time

data Statistics = Statistics {
  definiteTime  :: Double,
  definiteStart :: UTCTime,
  
  possibleTime  :: Double,
  possibleStart :: UTCTime,
  
  generalizeTime  :: Double,
  generalizeStart :: UTCTime,

  wideningTime  :: Double,
  wideningStart :: UTCTime,

  countingTime  :: Double,
  countingStart :: UTCTime,
  
  totalTime  :: Double,
  totalStart :: UTCTime,

  generalizeCount :: Int,
  solverCallCount :: Int,
  iterationStat :: Int,

  partitionCount :: Int,
  exactPartitionCount :: Int,

  conjunctsSlicedAway :: Int
  } deriving (Show, Read)
