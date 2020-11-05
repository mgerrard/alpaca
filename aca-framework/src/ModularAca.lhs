\begin{code}
{-

A run of modular ACA takes a program and one of its
function names, and computes the precondition(s) on
this function whose satisfaction *may* or *must*
lead to an assertion violation.

To compute these preconditions, we (1) make any
nonlocal referenced variables (i.e., globals or
parameters) symbolic, (2) wrap this setup and the
body of the function-of-interest with a call to
main, and (3) call ACA on this modified program.
(Note we throw away the original main declaration but
keep all other declarations as they may be referenced
within the function-of-interest; this assumes that
the original main is not reentrant, i.e., no other
functions call main.)

-}

modularAca :: Program -> String -> AcaComputation Csc
modularAca p name = do
  maybeF <- findFunction p name
  error ("Cannot find function "++name) when (isNothing maybeF)
  let (Just f) = maybeF

  setup <- makeVarsSymbolic f
  main' <- wrapInMain [setup, (bodyOf f)]
  p' <- replaceMain p main'
  csc <- aca p' emptyCsc

  return csc

{-

The only referenced variables we need to make
symbolic are either globals or the parameters
of the function-of-interest. This will use the
function analyzeAST from the language-c library,
which, given an AST, returns a map of global
declarations.

-}

determineVarsOfInterest :: CTranslUnit -> CFunDef -> M.Map Ident TypeDef
determineVarsOfInterest ast f = error "need to implement determineVarsOfInterest"
{-
  -- need to make this an instance of MonadTrav
  globalMap <- analyzeAST ast
  params <- getParameters f
  vs <- getReferencedVars f
  varsOfInterest <- [params]++(globals globalMap) `intersect` vs
  ...
  return varsOfInterest
-}

makeSymbolic :: Variable -> CStat
makeSymbolic (BaseType v) = makeBaseSymbolic v
makeSymbolic (PtrToBaseType v) = makePtrToBaseSymbolic v
makeSymbolic (StructType v) = makeStructSymbolic v
makeSymbolic (PtrToStructType v) = makePtrToStructSymbolic v

makeVarsSymbolic :: [Variable] -> CStat
makeVarsSymbolic vs = error "need to implement makeVarsSymbolic"
{-
  0. determine vars of interest
  1. declare the parameters
  2. map make symbolic across the set of variables-of-interest
  3. return this compound statement
-}
\end{code}
