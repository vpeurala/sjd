module Sjd (runMain, test1) where

import qualified Parser as P
import Model as M
import Transform as T
import JavaCommon as JC
import qualified JavaGenerator as JG
import JavaBuilderGenerator as JBG
import ScalaGenerator as SG

import Data.List (intercalate)
import Data.List.Split
import Data.Monoid ((<>))
import Text.ParserCombinators.Parsec
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

parseFile :: String -> IO (Either ParseError P.CodebaseDeclaration)
parseFile fileName = do
  content <- readFile fileName
  return $ P.parseCodebase fileName content

runMain :: IO ()
runMain = do
  args <- getArgs
  parseResult <- parseFile (head args)
  case parseResult of
    (Left parseError) -> print parseError
    (Right codebaseDeclaration) -> process $ transform codebaseDeclaration

process :: M.Codebase -> IO ()
process codebase =
  let javaSources = JC.generate (JavaClassSpecificGenerator JG.generateJavaClass JG.importsFromFieldType JG.classImports) codebase
      builderSources = JC.generate (JavaClassSpecificGenerator JBG.generateJavaClass JBG.importsFromFieldType JBG.classImports) codebase
      scalaSources = JC.generate (ScalaPackageSpecifigGenerator SG.generateScalaPackage) codebase
      javaDir = "src/main/java"
      scalaDir = "src/main/scala"
  in  do
      mapM_ (\(JavaSource fqn sourceCode) -> do
        createDirectoryIfMissing True (javaDir <> "/" <> fqnToPackageDir fqn)
        writeFile (javaDir <> "/" <> fqnToFile fqn <> ".java") sourceCode
            ) javaSources
      mapM_ (\(JavaSource fqn sourceCode) -> do
        createDirectoryIfMissing True (javaDir <> "/" <> fqnToPackageDir fqn)
        writeFile (javaDir <> "/" <> fqnToFile fqn <> "Builder.java") sourceCode
            ) builderSources
      mapM_ (\(ScalaSource fqn sourceCode) -> do
              createDirectoryIfMissing True (scalaDir <> "/" <> fqnToPackageDir fqn)
              writeFile (scalaDir <> "/" <> fqnToFile fqn <> ".scala") sourceCode
                  ) scalaSources

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
