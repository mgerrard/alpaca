module Widening where

import Data.List
import Data.Maybe (isJust, catMaybes)
import Data.Matrix
import Octagon
import CscTypes
import Language.C

data DBM = DBM {
  variables :: [Variable],
  dbmMatrix :: Matrix IntPlusInf
  } deriving (Eq, Show)

data PotentialConstraint = PotentialConstraint {
  v_i :: Variable,
  v_j :: Variable,
  constraint :: Integer
  } deriving (Eq, Show)

{- Based of the DBM definition of:
     c      if (v_j - v_i <= c) in C
     +\inf  elsewhere. 
-}
getDbmEntry :: [PotentialConstraint] -> Variable -> Variable -> IntPlusInf
getDbmEntry cs v1 v2  =
  let maybeConstraint = lookupConstraint (v1, v2) cs
  in
    if (isJust maybeConstraint)
      then
        let (Just c) = maybeConstraint
        in Number c
      else Infinity

{- The switch of i and j is intentional; see def'n above -}
lookupConstraint :: (Variable, Variable) -> [PotentialConstraint] -> Maybe Integer
lookupConstraint (v1, v2) cs =
  let maybeC = find (\(PotentialConstraint i j _)->((v1==j)&&(v2==i))) cs
  in
    if (isJust maybeC)
      then
        let (Just (PotentialConstraint _ _ c)) = maybeC
        in (Just c)
      else Nothing

data IntPlusInf = Number Integer | Infinity deriving (Eq)
instance Show IntPlusInf where
  show (Number i) = (show i)
  show Infinity = "+∞"
instance Ord IntPlusInf where
  Infinity `compare` Infinity = EQ
  (Number _) `compare` Infinity = LT
  Infinity `compare` (Number _) = GT
  (Number a) `compare` (Number b) = a `compare` b

{- A Constraint given by DIG is always of the form of either
    "x + y < bound" or "x < bound" -}
data DigConstraint = DigConstraint {
  leftVar :: Variable,
  maybeRightVar :: Maybe Variable,
  bound :: Integer
  } deriving (Eq, Show)

{- We subtract one from the bound to turn the '<' relation to '<='
   And we follow the translation of Fig. 5 from translating
   between constraints over V+ and their coherent form in V.-}
toPotentialForm :: OctConstraint -> [PotentialConstraint]
toPotentialForm (OctConstraint (SignedOneVar lVar) b) = [makeCoherentPotentialConstraint lVar (toInteger b)]
toPotentialForm (OctConstraint (SignedTwoVar lVar rVar) b) = makeTwoCoherentPotentialConstraints lVar rVar ((toInteger b)-1)

makeCoherentPotentialConstraint :: Variable -> Integer -> PotentialConstraint
{-  v_i < c <=> v_i <= (c-1) maps to
    v_i+ - v_i- <= 2*(c-1)
-}
makeCoherentPotentialConstraint (Var v Pos) c = PotentialConstraint (Var v Pos) (Var v Neg) (2*(c-1))
{-  -v_i < c <=> v_i >=c  maps to
    v_i- - v_i+ <= -2c -}
makeCoherentPotentialConstraint (Var v Neg) c = PotentialConstraint (Var v Neg) (Var v Pos) ((-2)*c)
makeCoherentPotentialConstraint Zero _ = error "I shouldn't be making a potential constraint with a zero variable."

makeTwoCoherentPotentialConstraints :: Variable -> Variable -> Integer -> [PotentialConstraint]
makeTwoCoherentPotentialConstraints (Var vi Pos) (Var vj Neg) c =
  [ PotentialConstraint (Var vi Pos) (Var vj Pos) c,
    PotentialConstraint (Var vj Neg) (Var vi Neg) c ]
makeTwoCoherentPotentialConstraints (Var vi Pos) (Var vj Pos) c =
  [ PotentialConstraint (Var vi Pos) (Var vj Neg) c,
    PotentialConstraint (Var vj Pos) (Var vi Neg) c ]
makeTwoCoherentPotentialConstraints (Var vi Neg) (Var vj Neg) c =
  [ PotentialConstraint (Var vj Neg) (Var vi Pos) c,
    PotentialConstraint (Var vi Neg) (Var vj Pos) c ]
makeTwoCoherentPotentialConstraints vi@(Var _ Neg) vj@(Var _ Pos) c = makeTwoCoherentPotentialConstraints vj vi c
makeTwoCoherentPotentialConstraints _ _ _ = error "I shouldn't be making a potential constraint with a zero variable."

{-
  Miné 2009: We consider that each variable v_i in V+
  comes in two flavors: a positive form v_i+ and a
  negative form v_i-. We introduce the set
    V = { v_0+, v_0-,...,v_{N-1}+, v_{N-1}- }
-}
potentialVarDomain :: [Variable] -> [Variable]
potentialVarDomain vs =
  let vs' = map (\v@(Var name sign)->[v, (Var name (opposite sign))])  vs
  in nub $ concat vs'

opposite :: Sign -> Sign
opposite Pos = Neg
opposite Neg = Pos
                  
display :: DBM -> IO ()
display (DBM _ mat) = do
  putStrLn (prettyMatrix mat)
  putStrLn ""

displayStr :: DBM -> String
displayStr (DBM _ mat) = prettyMatrix mat

{- Widening is just a transformation on DBM -}

widen :: DBM -> DBM -> DBM
widen (DBM v1 m1) (DBM _ m2) =
  let size = length v1
      dbmW = matrix size size $ \(i,j) -> (getElem i j m1) `widenElem` (getElem i j m2)
  in (DBM v1 dbmW)

widenElem :: IntPlusInf -> IntPlusInf -> IntPlusInf
widenElem m_ij n_ij =
  if n_ij <= m_ij
    then m_ij
    else Infinity

{- There may be some redundancy in the potential constraints -}
dbmToPotentialConstraints :: DBM -> [PotentialConstraint]
dbmToPotentialConstraints (DBM vars dbm) =
  let idxs = [1..(nrows dbm)]
      cs = map (\i-> (map (\j-> makeConstraint vars i j (getElem i j dbm)) idxs)) idxs
  in catMaybes $ concat cs

potentialToConjunct :: PotentialConstraint -> Conjunct
potentialToConjunct (PotentialConstraint vi vj c) =
  Conjunct (CBinary CLeqOp (CBinary CSubOp (mkVarExpr vi) (mkVarExpr vj) undefNode) (mkConst (fromIntegral c)) undefNode)

mkVarExpr :: Variable -> CExpr
mkVarExpr (Var v Neg) = CUnary CMinOp (makeCIndex v) undefNode
mkVarExpr (Var v Pos) = makeCIndex v
mkVarExpr Zero = error "Should not be here."

makeConstraint :: [Variable] -> Int -> Int -> IntPlusInf -> Maybe PotentialConstraint
makeConstraint _ _ _ Infinity = Nothing
{- Switching the order of i and j is intentional -}
makeConstraint vs i j (Number n) = Just (PotentialConstraint (vs !! (j-1)) (vs !! (i-1)) n)

{- The second potential constraint is passed in to ensure
   the two matrices being made are of the same size -}
makeDbm :: [PotentialConstraint] -> [PotentialConstraint] -> DBM
makeDbm ps1 ps2 =
  let vars = nub $ (collectVars ps1) ++ (collectVars ps2)
      arrayMatrix = map (\v1-> (map (\v2-> getDbmEntry ps1 v1 v2) vars)) vars
      mat = fromLists arrayMatrix
  in (DBM vars mat)

collectVars :: [PotentialConstraint] -> [Variable]
collectVars ps =
  let vs = concat $ map (\(PotentialConstraint v1 v2 _) ->[v1,v2]) ps
  in potentialVarDomain vs      
