-- Based on examples at https://wiki.haskell.org/HXT/Practical
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Branch where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Control.Exception

data Branch = Branch
  { line   :: Int,
    guide  :: Bool,
    source :: Maybe String }
  deriving (Show, Eq)

getBranches :: Bool -> String -> IO (Maybe [Branch])
getBranches True file = do
  o <- runX (parseXML file >>> makeSourceBranches); return (Just o)
  `catch`
  (\e -> do (putStrLn (show (e::SomeException))); return Nothing)
getBranches _ file = do
  o <- runX (parseXML file >>> makeBranches); return (Just o)
  `catch`
  (\e -> do (putStrLn (show (e::SomeException))); return Nothing)

parseXML :: String -> IOStateArrow s b XmlTree
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

makeBranches :: ArrowXml cat => cat (NTree XNode) Branch
makeBranches = getBranchEdges >>>
  proc b -> do
    l <- text <<< atKeyValue "startline" -< b
    branchTaken <- text <<< atKeyValue "control" -< b
    returnA -< Branch {
      line = read l :: Int,
      guide = getBool branchTaken,
      source = Nothing }

makeSourceBranches :: ArrowXml cat => cat (NTree XNode) Branch
makeSourceBranches = getBranchEdges >>>
  proc b -> do
    l <- text <<< atKeyValue "startline" -< b
    branchTaken <- text <<< atKeyValue "control" -< b
    sourceCode <- text <<< atKeyValue "sourcecode" -< b
    returnA -< Branch {
      line = read l :: Int,
      guide = getBool branchTaken,
      source = Just sourceCode }

getBranchEdges :: ArrowXml cat => cat (NTree XNode) XmlTree
getBranchEdges = atTag "edge" >>>
  proc l -> do
    hasAttrValue "target" (/="sink") -< l
    dat  <- atTag "data" -< l
    hasAttrValue "key" (=="control") -< dat
    returnA -< l

atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText

atKeyValue :: ArrowXml a => String -> a (NTree XNode) XmlTree
atKeyValue v = deep (isElem >>> hasAttrValue "key" (==v))

getBool :: String -> Bool
getBool s
  | s == "condition-true" = True
  | otherwise = False
