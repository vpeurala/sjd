module Sjd (runMain, test1) where

import qualified Parser as P
import Model as M
import Transform as T
import JavaCommon as JC
import JavaGenerator as JG
import JavaBuilderGenerator as JBG

import Data.List (intercalate)
import Data.List.Split
import Text.ParserCombinators.Parsec
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

parseFile :: String -> IO (Either ParseError P.CodebaseDeclaration)
parseFile fileName = do
  content <- readFile fileName
  return $ P.parseCodebase content

runMain :: IO ()
runMain = do
  args <- getArgs
  parseResult <- parseFile (head args)
  case parseResult of
    (Left parseError) -> print parseError
    (Right codebaseDeclaration) -> process $ transform codebaseDeclaration

process :: M.Codebase -> IO ()
process codebase =
  let javaSources = JC.generate (ClassSpecificGenerator JG.generateJavaClass JG.importsFromFieldType) codebase
      builderSources = JC.generate (ClassSpecificGenerator JBG.generateJavaClass JBG.importsFromFieldType) codebase
  in  do
      mapM_ (\(JavaSource fqn sourceCode) -> do
        createDirectoryIfMissing True ("src/main/java/" ++ fqnToPackageDir fqn)
        writeFile ("src/main/java/" ++ fqnToFile fqn ++ ".java") sourceCode
            ) javaSources
      mapM_ (\(JavaSource fqn sourceCode) -> do
        createDirectoryIfMissing True ("src/main/java/" ++ fqnToPackageDir fqn)
        writeFile ("src/main/java/" ++ fqnToFile fqn ++ "Builder.java") sourceCode
            ) builderSources

fqnToPackageDir :: FullyQualifiedClassName -> String
fqnToPackageDir fqn =
  let ds = splitOn "." fqn
  in  intercalate "/" (init ds)

fqnToFile :: FullyQualifiedClassName -> String
fqnToFile fqn =
  let ds = splitOn "." fqn
  in  intercalate "/" ds

test1 :: IO Codebase
test1 = do
  p <- parseFile "test/customer.sjd"
  let (Right pp) = p
  return $ transform pp
