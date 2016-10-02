{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module JavaCommon where

import Control.Monad.Reader
import Data.List (find, nub)
import Data.Monoid ((<>))

import qualified Model as M

class ReaderEnv a where
  getGenerator :: a -> Generator
  getCodebase  :: a -> M.Codebase
  getPackage   :: a -> M.Package

data ClassReaderEnv = ClassReaderEnv {
                        getGenerator1 :: Generator,
                        getCodebase1  :: M.Codebase,
                        getPackage1   :: M.Package,
                        getClass      :: M.Class
                      }

instance ReaderEnv ClassReaderEnv where
    getGenerator = getGenerator1
    getCodebase  = getCodebase1
    getPackage   = getPackage1

data PackageReaderEnv = PackageReaderEnv {
                          getGenerator2 :: Generator,
                          getCodebase2  :: M.Codebase,
                          getPackage2   :: M.Package
                        }

toClassReaderEnv :: PackageReaderEnv -> M.Class -> ClassReaderEnv
toClassReaderEnv (PackageReaderEnv {getGenerator2, getCodebase2, getPackage2}) c = ClassReaderEnv { getGenerator1 = getGenerator2
                                                                                                  , getCodebase1  = getCodebase2
                                                                                                  , getPackage1   = getPackage2
                                                                                                  , getClass      = c }

instance ReaderEnv PackageReaderEnv where
    getGenerator = getGenerator2
    getCodebase  = getCodebase2
    getPackage   = getPackage2

type ClassReader = Reader ClassReaderEnv

type PackageReader = Reader PackageReaderEnv

data Generator = JavaClassSpecificGenerator {
                   generateSourceForClass :: Generator -> M.Codebase -> M.Package -> M.Class -> M.JavaSource
                 , importsFromFieldType :: M.FieldType -> ClassReader [M.Import]
                 , classImports :: ClassReader [M.Import]
                 }
                 | ScalaPackageSpecificGenerator {
                   generateSourcesForPackage :: Generator -> M.Codebase -> M.Package -> M.ScalaSource
                 }

generate :: Generator -> M.Codebase -> [M.JavaSource]
generate generator codebase@(M.Codebase packages) =
  packageToSources generator codebase =<< packages

packageToSources :: Generator -> M.Codebase -> M.Package -> [M.JavaSource]
packageToSources generator codebase package@(M.Package _ classes) =
  fmap (generateSourceForClass generator generator codebase package) classes

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
  M.List fieldType'     -> "List<" <> javaize fieldType' <> ">"
  M.Optional fieldType' -> "Optional<" <> javaize fieldType' <> ">"
  M.Object className    -> className

isDomainType :: M.FieldType -> ClassReader Bool
isDomainType (M.Object className) = do
  M.Codebase packages <- asks getCodebase
  return $ any (\(M.Package _ classes) -> any (\(M.Class _ name _ _ _) -> name == className) classes) packages
isDomainType _ = return False

findDomainClass :: M.ClassName -> ClassReader (Maybe M.Class)
findDomainClass searchedClassName = do
  M.Codebase packages <- asks getCodebase
  let allClasses = (\(M.Package _ classes) -> classes) =<< packages
  return $ find (\(M.Class _ className _ _ _) -> className == searchedClassName) allClasses

getAllFields :: M.Class -> ClassReader [M.Field]
getAllFields klass@(M.Class _ _ _ _ fields) = do
  allSuperClassFields <- getSuperClassFields klass
  return $ allSuperClassFields <> fields

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
  return $ fmap (\(M.Package (Just packageName) _) -> packageName <> "." <> className) packages
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
  klass@(M.Class imports _ _ _ _) <- asks getClass
  allFields <- getAllFields klass
  calculatedImports <- calculateImportsFromFields allFields
  classImports' <- classImports generator
  let allImports = nub $ imports <> calculatedImports <> classImports'
  return $ (\imp -> "import " <> imp <> ";\n") =<< allImports

calculateImportsFromFields :: [M.Field] -> ClassReader [M.Import]
calculateImportsFromFields fields = do
  generator <- asks getGenerator
  importsFromFields <- mapM (importsFromFieldType generator . (\(M.Field _ fieldType) -> fieldType)) fields
  return $ concat importsFromFields
