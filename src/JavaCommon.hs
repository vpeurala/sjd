module JavaCommon where

import Control.Monad.Reader
import Data.List (find, nub)

import qualified Model as M

data ClassReaderEnv = ClassReaderEnv {
                        getGenerator :: Generator,
                        getCodebase  :: M.Codebase,
                        getPackage   :: M.Package,
                        getClass     :: M.Class
                      }

type ClassReader a = Reader ClassReaderEnv a

data Generator = ClassSpecificGenerator {
                   generateSourceForClass :: Generator -> M.Codebase -> M.Package -> M.Class -> M.JavaSource,
                   importsFromFieldType :: M.FieldType -> ClassReader [M.Import],
                   classImports :: ClassReader [M.Import]
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
  M.Codebase packages <- asks getCodebase
  return $ any (\(M.Package _ classes) -> any (\(M.Class _ name _ _ _) -> name == className) classes) packages
isDomainType _ = return False

findDomainClass :: M.ClassName -> ClassReader (Maybe M.Class)
findDomainClass searchedClassName = do
  M.Codebase packages <- asks getCodebase
  let allClasses = concatMap (\(M.Package _ classes) -> classes) packages
  return $ find (\(M.Class _ className _ _ _) -> className == searchedClassName) allClasses

getAllFields :: M.Class -> ClassReader [M.Field]
getAllFields klass@(M.Class _ _ _ _ fields) = do
  allSuperClassFields <- getSuperClassFields klass
  return $ allSuperClassFields ++ fields

getSuperClassFields :: M.Class -> ClassReader [M.Field]
getSuperClassFields (M.Class _ _ maybeExtends _ _) =
  case maybeExtends of
    Just superClassName -> do
      superClass <- findDomainClass superClassName
      case superClass of
        (Just superClass') -> getAllFields superClass'
        Nothing            -> return []
    Nothing             -> return []

packagesForClass :: M.FieldType -> ClassReader [M.Package]
packagesForClass (M.Object className) = do
  M.Codebase packages <- asks getCodebase
  return $ filter (\(M.Package _ classes) -> any (\(M.Class _ name _ _ _) -> name == className) classes) packages
packagesForClass _ = error "This should be unreachable code"

fqns :: M.FieldType -> ClassReader [M.FullyQualifiedClassName]
fqns fieldType@(M.Object className) = do
  packages <- packagesForClass fieldType
  return $ map (\(M.Package (Just packageName) _) -> packageName ++ "." ++ className) packages
fqns _ = error "This should be unreachable code"

needsImport :: M.FieldType -> ClassReader Bool
needsImport ft = do
  package <- asks getPackage
  isDomainType' <- isDomainType ft
  packagesForClass' <- packagesForClass ft
  let isInDifferentPackage' = package `notElem` packagesForClass'
  return (isDomainType' && isInDifferentPackage')

-- TODO vpeurala 9.12.2015: Use maybeExtends and implements
importDeclarations :: ClassReader M.SourceCode
importDeclarations = do
  generator <- asks getGenerator
  M.Class imports _ _ _ fields <- asks getClass
  calculatedImports <- calculateImportsFromFields fields
  classImports' <- classImports generator
  let allImports = nub $ imports ++ calculatedImports ++ classImports'
  return $ concatMap (\imp -> "import " ++ imp ++ ";\n") allImports

calculateImportsFromFields :: [M.Field] -> ClassReader [M.Import]
calculateImportsFromFields fields = do
  generator <- asks getGenerator
  importsFromFields <- mapM (importsFromFieldType generator . (\(M.Field _ fieldType) -> fieldType)) fields
  return $ concat importsFromFields
