import Options.Applicative
import Data.Semigroup ((<>))

data CStatement = 
  IfS Condition CStatement
  | IfElseS Condition CStatement CStatement
  | ReadS Variable
  | ErrorS
  | ReturnS

instance Show CStatement where
  show (ErrorS) = "__VERIFIER_error();\n"
  show (ReadS var) = "int "++(show var)++" = __VERIFIER_nondet_int();\n"
  show (IfS c stat) = "if ("++(show c)++") {\n"++(show stat)++"}"
  show (IfElseS c thenS elseS) =
    "if ("++(show c)++") {\n"++(show thenS)++"} else {\n"++(show elseS)++"}"
  show (ReturnS) = "return 0;"

makeTree :: Int -> Int -> (Int,Int) -> (Variable,Variable) -> CStatement
makeTree depth location t@(maxDepth,targetLeaf) (v1,v2) =
  if depth == maxDepth
    then
      if location == targetLeaf
        then IfS (BinExpr (VarExpr v1) EqOp (Constant 42)) ErrorS
        else ReturnS
    else (IfElseS (BinExpr (VarExpr v1) EqOp (VarExpr v2))
          (makeTree (depth+1) ((location*2)-1) t (v1,(incrVar v2)))
          (makeTree (depth+1) (location*2) t (v2,(incrVar v2))))

varId :: Variable -> Int
varId (Variable i) = i

incrVar :: Variable -> Variable
incrVar (Variable i) = (Variable (i+1))

ifCascade :: Bool -> CStatement -> [Variable] -> CStatement
ifCascade False s vs = ifCascadeEq s vs
ifCascade True s vs = ifCascadeRand True s vs

ifCascadeEq :: CStatement -> [Variable] -> CStatement
ifCascadeEq s vs@(v1:v2:_) =
  IfS (BinExpr (VarExpr v1) EqOp (VarExpr v2)) (ifCascadeEq s (tail vs))
ifCascadeEq s (v:[]) = IfS (BinExpr (VarExpr v) EqOp (Constant 42)) s
ifCascadeEq _ [] = error "the cascade of ifs expects some variable"

ifCascadeRand :: Bool -> CStatement -> [Variable] -> CStatement
ifCascadeRand False s vs@(v1:v2:_) =
  IfS (Neg (BinExpr (VarExpr v1) EqOp (VarExpr v2))) (ifCascadeRand True s (tail vs))
ifCascadeRand True s vs@(v1:v2:_) =
  IfS (BinExpr (VarExpr v1) EqOp (VarExpr v2)) (ifCascadeRand False s (tail vs))
ifCascadeRand _ s (v:[]) = IfS (BinExpr (VarExpr v) EqOp (Constant 42)) s
ifCascadeRand _ _ [] = error "the cascade of ifs expects some variable"
  
type Condition = Expr
data Variable = Variable Int

instance Show Variable where
  show (Variable i) = "x"++(show i)
  
data Expr = 
  VarExpr Variable
  | Constant Int
  | BinExpr Expr Op Expr
  | Neg Expr

instance Show Expr where
  show (VarExpr v) = (show v)
  show (Constant i) = show i
  show (BinExpr lhs op rhs) = (show lhs)++(show op)++(show rhs)
  show (Neg e) = "!("++(show e)++")"
    
data Op = EqOp
  
instance Show Op where
  show (EqOp) = "=="
  
progHeader :: String
progHeader =
  "extern void __VERIFIER_error() __attribute__ ((__noreturn__));\n\
  \extern int __VERIFIER_nondet_int();\n\
  \int main() {\n"

progFooter :: String
progFooter = "return 0;}"

main :: IO ()
main = do
  synthesize =<< execParser opts
  return ()

data Config = Config
  { varNumParam :: Int
  , leafParam :: Int
  , skinnyParam :: Bool
  , randomParam :: Bool
  }

opts :: ParserInfo Config
opts = info ( configuration <**> helper )
  (  fullDesc
  <> header   "synth-c -- synthesize a C program" )

configuration :: Parser Config
configuration = Config
  <$> option auto
      (  long "height"
      <> short 'h'
      <> help "Specify height of tree"
      <> metavar "INT" )
  <*> option auto
      (  long "leaf"
      <> short 'l'
      <> help "Specify leaf in tree to place reachable statement"
      <> value 1
      <> showDefault
      <> metavar "INT" )
  <*> switch
      (  long "skinny"
      <> short 's'
      <> help "Skinny tree (a simple cascade)? (default is dense tree)" )
  <*> switch
      (  long "randomize"
      <> short 'r'
      <> help "Randomize branch outcomes" )

constructStmt :: Config -> [Variable] -> CStatement
constructStmt (Config _ _ True r) vars = (ifCascade r ErrorS vars)
constructStmt (Config d l False _) (v1:v2:_) = (makeTree 1 1 (d,l) (v1,v2))
constructStmt _ _ = error "expecting at least 2 variables"

synthesize :: Config -> IO ()
synthesize config@(Config n _ _ _) = do
  let vars = map (\i -> (Variable i)) (take n [1..])
      readStmts = map (\c -> (ReadS c)) vars
      stmts = readStmts++[(constructStmt config vars)]
      program = progHeader++(concat $ map show stmts)++progFooter

  putStrLn program
