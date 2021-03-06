\begin{code}
module Configuration where

data Configuration = Configuration
  { fileParam :: String
  , debugParam :: String
  , timeoutParam :: Int
  , portfolioParam :: String
  , generalizeTimeoutParam :: Int
  , blockValidPathsParam :: Bool
  , exitStrategyParam :: String
  , genExitStratParam :: String
  , prefixParam :: String
  , targetFunctionParam :: String
  , partitionBoundParam :: Int
  , mergeLengthParam :: Int
  , genStratParam :: String
  , cppParam :: String
  , initTimeoutParam :: Int
  , excludeParam :: String
  , dseParam :: String
  , makeCudParam :: Bool
  , chewCudParam :: String
  , dockerParam :: Bool
  , minusAcaParam :: Bool
  , propertyParam :: String
  } deriving (Show)

data CountConfiguration = CountConfiguration { cscFileToCount :: String }
\end{code}
