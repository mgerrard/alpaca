\begin{code}
module CivlParsing where

import Data.List.Split (splitOn, split, oneOf)
import Data.List (sortBy, groupBy, nub, nubBy, sort, partition, isInfixOf, find)
import Data.Function (on)
import Control.Monad (join)
import Control.Exception
import Data.String.Utils (replace)
import qualified Data.Map.Strict as Map
import CscTypes

data CivlOutput = CivlOutput {
  symbolicToConcrete :: [SymbolicConcrete],
  rawPathConditionTriple :: [RawPcTriple],
  rawAssumptionsTriple :: [RawPcTriple],
  numberSliced :: Int
  } deriving (Show)

data SymbolicConcrete = SymbolicConcrete {
  symbolicName :: String,
  symbolicType :: TypeString,
  concreteId   :: Int
  } deriving (Show)

data RawPcTriple = RawPcTriple {
  rawCeePc    :: String,
  rawZ3Pc     :: String,
  rawVarTypes :: String
  } deriving (Show)

parseCivlOutput :: CivlOutput -> PreSubspace
parseCivlOutput civlOut = 
  let inputMap  = deriveInputMap $ symbolicToConcrete civlOut
      countMap  = inputCountsById inputMap
      typeMap   = inputTypesById inputMap
      ceePcs    = map rawCeePc $ rawPathConditionTriple civlOut
      ceeConjs  = translateCeePc inputMap ceePcs
      conj      = map RawConjunct ceeConjs
      conj'    = nub conj
      conj''   = inferEquality conj'
      {- Normalize conjunction by sorting conjuncts lexicographically -}
      conj'''   = sortBy (\(RawConjunct c1) (RawConjunct c2) -> compare c1 c2) conj''
      assumePcs = map rawCeePc $ rawAssumptionsTriple civlOut
      assumeConjs  = translateCeePc inputMap assumePcs
      a     = map RawConjunct assumeConjs
      a'    = nub a
      a''   = inferEquality a'
      {- Normalize conjunction by sorting conjuncts lexicographically -}
      a'''   = sortBy (\(RawConjunct c1) (RawConjunct c2) -> compare c1 c2) a''
      rConj = RawConjunction conj''' (numberSliced civlOut)
      aConj = RawConjunction a''' 0
  in PreSubspace rConj aConj countMap typeMap

{- A brittle hack -}
inferEquality :: [RawConjunct] -> [RawConjunct]
inferEquality cs@[(RawConjunct cc1), (RawConjunct cc2)] =
  let cc1' = sort $ splitOn " <= " $ init $ tail $ cc1
      cc2' = sort $ splitOn " <= " $ init $ tail $ cc2
  in
    if (cc1' == cc2')
      then
        let ccEq = "(0 == "++(last cc1')++")"
        in [(RawConjunct ccEq)]
      else cs
inferEquality cs = cs

inputCountsById :: [InputMapping] -> Map.Map Int Int
inputCountsById m =
  let uniqueIds = nubBy (\x y -> (acaInputId x)==(acaInputId y)) m
      aList = map (\y -> ((acaInputId y), (iterationCount y))) uniqueIds
  in Map.fromList aList

inputTypesById :: [InputMapping] -> Map.Map Int TypeString
inputTypesById m =
  let uniqueIds = nubBy (\x y -> (acaInputId x)==(acaInputId y)) m
      aList = map (\y -> ((acaInputId y), (symbolicInputType y))) uniqueIds
  in Map.fromList aList

convertVarTypeTuples :: [String] -> [(String, String)]
convertVarTypeTuples [] = []
convertVarTypeTuples (k:v:t) = (k, v) : convertVarTypeTuples t
convertVarTypeTuples _ = error "Unhandled case in convertVarTypeTuples"
  
translateCeePc :: [InputMapping] -> [String] -> [String]
translateCeePc inputMap pc =
  let assocMap  = map makeSymConcTuple inputMap
      cs = map (transformConjunct assocMap) pc
      cs' = map (\c -> "("++c++")") cs
  in cs'

symvarTransformers :: [(String, String)] -> [(String -> String)]
symvarTransformers assocMap =
  map (\y -> (replace (fst y) (snd y))) assocMap

transformConjunct :: [(String, String)] -> String -> String
transformConjunct assocMap conj =
  let t    = (\word pair -> if (fst pair)==word then (snd pair) else word)
      cs   = (split . oneOf) " %()*/" conj
      csP = pointerTransform cs
      cs'  = map (\word -> foldl t word assocMap) csP
      cs''  = wrapBitwiseAnd cs'
      cs''' = simplifyTernaryTautology $ concat cs''
      cs'''' = removeIdentityMultOp cs'''
      cs''''' = replaceDivOp cs''''
  in cs'''''

{-
The following hack relies on these assumptions:
 - we are treating symbolic pointers in a boolean way:
   + the pointer is either NULL
   + or the pointer is *not* NULL
 - CIVL will report a NULL pointer in the regex form:
   + 0 != *SYMVAR.*&&*
   + we translate this to: SYMVAR == 0
 - CIVL will report a non-NULL pointer in the regex form:
   + 0 != *SYMVAR.*||*
   + we translate this to: SYMVAR != 0
-}
pointerTransform :: [String] -> [String]
pointerTransform s =
  if ("||" `elem` s) && (any (\sub -> '.' `elem` sub) s)
    then
      let (Just civlSym) = find (\sub -> '.' `elem` sub) s
          symElem = takeWhile (\c -> c /= '.') civlSym
      in [symElem,"!=","0"]
    else 
      if ("&&" `elem` s) && (any (\sub -> '.' `elem` sub) s)
        then
          let (Just civlSym) = find (\sub -> '.' `elem` sub) s
              symElem = takeWhile (\c -> c /= '.') civlSym
          in [symElem,"==","0"]
        else s

wrapBitwiseAnd :: [String] -> [String]
wrapBitwiseAnd s =
  if "&" `elem` s
    then
      let ss = splitOn ["&"] s
          ss1 = (head ss)
          ss1' = (init (init ss1))
          ss1'' = ["(", (last (init ss1))]
          ss2 = (last ss)
          ss2' = [(head (tail ss2)), ")"]
          ss2'' = (tail (tail ss2))
          ss3 = ss1' ++ ss1'' ++ [" & "] ++ ss2' ++ ss2''
      in ss3
    else s

removeIdentityMultOp :: String -> String
removeIdentityMultOp s =
  if " 1*" `isInfixOf` s
    then replace " 1*" " " s
    else s

replaceDivOp :: String -> String
replaceDivOp s =
  if " div " `isInfixOf` s
    then replace " div " " / " s
    else s

{- WARNING: giant hack incoming!

    CIVL outputs its conjuncts involving a boolean expression
    in the confusing ternary form of:

     0 != ((expr) ? 1 : 0)
    or
     0 == ((expr) ? 1 : 0),

    which can be rewritten as 

     0 != (expr)
    or
     0 == (expr)

    So we can take the first five characters of this conjunct
    (the "0 != " or "0 == " part), the characters after the
    first parentheses, and do some reversing to chop off the
    repetitive ternary part.

    This is a terrible hack, but we can rely on it for now
    because CIVL always gives boolean tests in this strange form.

-}
simplifyTernaryTautology :: String -> String
simplifyTernaryTautology conjunct =
  let ternaryTautology = " ? 1 : 0)"
  in
    if ternaryTautology `isInfixOf` conjunct
      then
        let h = take 5 conjunct
            t = drop 6 conjunct
            t' = reverse t
            t'' = drop 9 t'
            t''' = reverse t''
            conjunct'' = h++t'''
            conjunct''' = secondOrderSimplification conjunct''
        in conjunct'''
      else conjunct

secondOrderSimplification :: String -> String
secondOrderSimplification ('0':' ':'=':'=':' ':'(':'0':' ':'=':'=':rest) = "(0 !="++rest
secondOrderSimplification ('0':' ':'!':'=':' ':'(':'0':' ':'=':'=':rest) = "(0 =="++rest
secondOrderSimplification ('0':' ':'=':'=':' ':'(':'0':' ':'!':'=':rest) = "(0 =="++rest
secondOrderSimplification ('0':' ':'!':'=':' ':'(':'0':' ':'!':'=':rest) = "(0 !="++rest
secondOrderSimplification s = s  

{- stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python -}
sliceString :: Int -> Int -> String -> String
sliceString from to xs = take (to - from + 1) (drop from xs)

makeSymConcTuple :: InputMapping -> (String, String)
makeSymConcTuple i@(InputMapping _ ty iden idx _) =
  let concreteName = makeConcreteName iden idx ty
  in ((symbolicInputVar i), concreteName)

makeConcreteName :: Int -> Int -> String -> String
makeConcreteName uniqueId index ty =
  let i = show uniqueId
      inx = show index
  in "aca_input_arr_"++ty++"_"++i++"["++inx++"]"

inputVariablesById :: ((Int, Int), String) -> [InputVariable]
inputVariablesById ((i, count), ty) =
  let indices = [0..(count-1)]
      names = map (\idx -> (makeConcreteName i idx ty)) indices
  in map (\n -> (n, ty)) names
  
deriveInputMap :: [SymbolicConcrete] -> [InputMapping]
deriveInputMap symToConcList =
  let idChunks = partitionByInputId symToConcList
  in join $ map makeInputMapping idChunks

partitionByInputId :: [SymbolicConcrete] -> [[SymbolicConcrete]]
partitionByInputId xs =
  let sortedXs = sortBy (compare `on` concreteId) xs
  in groupBy (\x y -> ((concreteId x) == (concreteId y))) sortedXs

makeInputMapping :: [SymbolicConcrete] -> [InputMapping]
makeInputMapping inputReads =
  let i      = concreteId $ assert (not (null inputReads)) $ head inputReads
      ty     = symbolicType $ head inputReads
      count  = length inputReads
      (freshReads, oldReads) = partition (isFreshRead . symbolicName) inputReads
      baseIndex = length oldReads
      freshInputs = makeFreshInputs i count (map symbolicName freshReads) ty baseIndex
      oldInputs = makeOldInputs i count (map symbolicName oldReads) ty
  in freshInputs ++ oldInputs

type InputId = Int
type InputCount = Int

makeFreshInputs :: InputId -> InputCount -> [String] -> TypeString -> Int -> [InputMapping]
makeFreshInputs i count varStrings ty baseIndex =
  let orderedVarStrings = sort varStrings
      indexTuples = zip orderedVarStrings [baseIndex..]
  in map (makeInputMappingWorker i count ty) indexTuples

makeInputMappingWorker :: InputId -> InputCount -> TypeString -> (String, Int) -> InputMapping
makeInputMappingWorker i count ty (symVar, index) =
  InputMapping { symbolicInputVar=symVar,
                 symbolicInputType=ty,
                 acaInputId=i,
                 inputArrayIndex=index,
                 iterationCount=count
               }

makeOldInputs :: InputId -> InputCount -> [String] -> TypeString -> [InputMapping]
makeOldInputs i count varStrings ty =
  let indexTuples = map deriveVarAndIndex varStrings
  in map (makeInputMappingWorker i count ty) indexTuples

isFreshRead :: String -> Bool
isFreshRead s = not $ (head s) == '['

deriveVarAndIndex :: String -> (String, Int)
deriveVarAndIndex varStr =
    nameAndIndexOfLastNonzeroElement varStr

nameAndIndexOfLastNonzeroElement :: String -> (String, Int)
nameAndIndexOfLastNonzeroElement varStr =
  let elems            = splitOn "," (init $ tail varStr)
      initializedElems = fst $ break (== "0") elems
      name             = last initializedElems
      index            = (length initializedElems) - 1
  in (name, index)

reduceCivlOutput :: String -> CivlOutput
reduceCivlOutput o =
  let symLines = grabSectionLines o "ACF Mapping"
      symConcs = map extractSymConcTriple symLines
      pcLines  = grabSectionLines o "Sliced PC"
      pcTriple = map extractPcTriple pcLines
      numLines = grabSectionLines o "Number sliced"
      number   = extractNumberSliced numLines
      asLines  = grabSectionLines o "Assumptions"
      asTriple = map extractPcTriple asLines
  in CivlOutput {
    symbolicToConcrete=symConcs,
    rawPathConditionTriple=pcTriple,
    rawAssumptionsTriple=asTriple,
    numberSliced=number
    }

extractNumberSliced :: [String] -> Int
extractNumberSliced [] = assert False 0
extractNumberSliced ss = read (head ss)::Int

extractSymConcTriple :: String -> SymbolicConcrete
extractSymConcTriple s =
  let elems   = words s
      symVar  = assert (not (null elems)) $ head elems
      concVar = last elems
      typeStr = extractInputType concVar
      concId  = extractInputId concVar
  in SymbolicConcrete symVar typeStr concId

extractPcTriple :: String -> RawPcTriple
extractPcTriple line =
  let elems  = splitOn " #Z3# " line
      cPc    = assert (not (null elems)) $ head elems
      elems' = splitOn " #TYPES# " (last elems)
      z3Pc   = assert (not (null elems')) $ head elems'
      varTys = last elems'
  in RawPcTriple cPc z3Pc varTys

extractInputType :: String -> TypeString
extractInputType varName =
  let elems = splitOn "_" varName
  in
    last $ init elems

extractInputId :: String -> Int
extractInputId varName =
  let elems = splitOn "_" varName
  in
    if (last elems) == "arr"
      then read (last (init elems))::Int
      else read (last elems)::Int

grabSectionLines :: String -> String -> [String]
grabSectionLines output keyword =
  let lParens  = "=== " ++ keyword ++ " ===\n"
      rParens  = "=== END ===\n"
      start    = tail $ splitOn lParens output
      -- this is nondeterministically failing when the entire
      -- portfolio is run;
      -- civl is always try to run the same replay
      start'   = assert (not (null start)) $ head start
      section  = splitOn rParens start'
      section' = assert (not (null section)) $ head section
  in lines section'
\end{code}
