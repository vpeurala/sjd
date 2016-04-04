module JavaCommon where

import Control.Monad.Reader
import Data.List (nub)

import qualified Model as M

type ClassReader a = Reader (Generator, M.Codebase, M.Package, M.ClassName, [M.Import], Maybe M.Extends, M.Implements, [M.Field]) a

data Generator = ClassSpecificGenerator {
                   generateSourceForClass :: Generator -> M.Codebase -> M.Package -> M.Class -> M.JavaSource,
                   importsFromFieldType :: M.FieldType -> ClassReader [M.Import]
                 }

generate :: Generator -> M.Codebase -> [M.JavaSource]
generate generator codebase@(M.Codebase packages) =
  concatMap (packageToSources generator codebase) packages

packageToSources :: Generator -> M.Codebase -> M.Package -> [M.JavaSource]
packageToSources generator codebase package@(M.Package _ classes) =
  map (generateSourceForClass generator generator codebase package) classes

javaize :: M.FieldType -> M.SourceCode
javaize fieldType = case fieldType of
  M.Boolean             -> "boolean"
  M.Byte                -> "byte"
  M.Short               -> "short"
  M.Char                -> "char"
  M.Int                 -> "int"
  M.Long                -> "long"
  M.Float               -> "float"
  M.Double              -> "double"
  M.List fieldType'     -> "List<" ++ javaize fieldType' ++ ">"
  M.Optional fieldType' -> "Optional<" ++ javaize fieldType' ++ ">"
  M.Object className    -> className

isDomainType :: M.FieldType -> ClassReader Bool
isDomainType (M.Object className) = do
  (_, M.Codebase packages, _, _, _, _, _, _) <- ask
  return $ any (\(M.Package _ classes) -> any (\(M.Class _ name _ _ _) -> name == className) classes) packages
isDomainType _ = return False

packagesForClass :: M.FieldType -> ClassReader [M.Package]
packagesForClass (M.Object className) = do
  (_, M.Codebase packages, _, _, _, _, _, _) <- ask
  return $ filter (\(M.Package _ classes) -> any (\(M.Class _ name _ _ _) -> name == className) classes) packages
packagesForClass _ = error "This should be unreachable code"

fqns :: M.FieldType -> ClassReader [M.FullyQualifiedClassName]
fqns fieldType@(M.Object className) = do
  packages <- packagesForClass fieldType
  return $ map (\(M.Package (Just packageName) _) -> packageName ++ "." ++ className) packages
fqns _ = error "This should be unreachable code"

needsImport :: M.FieldType -> ClassReader Bool
needsImport ft = do
  (_, _, package, _, _, _, _, _) <- ask
  isDomainType' <- isDomainType ft
  packagesForClass' <- packagesForClass ft
  let isInDifferentPackage' = package `notElem` packagesForClass'
  return (isDomainType' && isInDifferentPackage')

-- TODO vpeurala 9.12.2015: Use maybeExtends and implements
importDeclarations :: ClassReader M.SourceCode
importDeclarations = do
  (_, _, _, _, imports, maybeExtends, implements, fields) <- ask
  calculatedImports <- calculateImportsFromFields fields
  let allImports = imports ++ calculatedImports
    in return $ concatMap (\imp -> "import " ++ imp ++ ";\n") allImports

calculateImportsFromFields :: [M.Field] -> ClassReader [M.Import]
calculateImportsFromFields fields = do
  (generator, _, _, _, _, _, _, _) <- ask
  importsFromFields <- mapM (importsFromFieldType generator . (\(M.Field _ fieldType) -> fieldType)) fields
  return $ nub $ [ "java.util.Objects", "com.fasterxml.jackson.annotation.JsonCreator", "com.fasterxml.jackson.annotation.JsonProperty" ] ++ concat importsFromFields
