\begin{code}
module Parsing where
import System.IO.Silently
import Control.Arrow
import Control.Applicative
import qualified Data.Map as Map

import Language.C              -- simple API
import Language.C.Analysis     -- analysis API
import Language.C.System.GCC   -- preprocessor used

getAst :: FilePath -> IO CTranslUnit
getAst p = parseCFile (newGCC "gcc") Nothing [] p >>= checkResult "[parsing]"

makeSubProgram :: String -> String -> IO String
makeSubProgram file method = do
  subProgram <- wrapCallInMain file method
  let path = method ++ "_in_" ++ file
  writeFile path subProgram
  return path

wrapCallInMain :: String -> String -> IO String
wrapCallInMain file method = do
  (funcDef, _) <- capture $ printMethod file method
  return $ funcDef ++ "\nint main() {\n  " ++ method ++ "();\n}\n"

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

printMethod :: String -> String -> IO ()
printMethod c_file searchterm = do
  -- parse
  ast <- parseCFile (newGCC "gcc") Nothing [] c_file
    >>= checkResult "[parsing]"
  (globals,_warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
  let defId = searchDef globals searchterm
  -- traverse the AST and print decls which match
  case defId of
    Nothing -> putStrLn "Not found"
    Just def_id -> printDecl def_id ast
  where
  printDecl :: NodeInfo -> CTranslUnit -> IO ()
  printDecl def_id (CTranslUnit decls _) =
    let decls' = filter (maybe False (posFile (posOfNode def_id) ==).fileOfNode) decls in
    mapM_ (printIfMatch def_id) (zip decls' (map Just (tail decls') ++ [Nothing]))
  printIfMatch def (decl,Just next_decl) | posOfNode def >= posOf decl &&
                                           posOfNode def < posOf next_decl = (print . pretty) decl
                                         | otherwise = return ()
  printIfMatch def (decl, Nothing) | posOfNode def >= posOf decl = (print . pretty) decl
                                   | otherwise = return ()
  searchDef globs term =
    case analyseSearchTerm term of
      Left tag -> fmap nodeInfo (Map.lookup tag (gTags globs))
      Right ident ->     fmap nodeInfo (Map.lookup ident (gObjs globs))
                     <|> fmap nodeInfo (Map.lookup ident (gTypeDefs globs))
                     <|> fmap nodeInfo (Map.lookup (NamedRef ident) (gTags globs))
  analyseSearchTerm term =
    case words term of
      [tag,name] | tag `elem` (words "struct union enum") -> Left $ NamedRef (internalIdent name)
      [ident] -> Right (internalIdent ident)
      _ -> error "bad search term"
\end{code}
