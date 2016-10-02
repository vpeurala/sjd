module ScalaGenerator (generateScalaClass, importsFromFieldType, classImports) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as R
import qualified Data.List as L
import Data.Monoid ((<>))

import qualified JavaCommon as J
import qualified Model as M
import qualified Util as U

generateScalaPackage :: J.Generator -> M.Codebase -> M.Package -> M.ScalaSource
generateScalaPackage generator codebase package@(M.Package (Just packageName) _) =
  M.ScalaSource sourceCode
  where sourceCode       = U.separateNonBlanksWithNewline sourceOfParts <> "}"
        sourceOfParts    = fmap (`R.runReader` J.PackageReaderEnv generator codebase package) parts
        parts            = packageDeclaration : basicParts
generateScalaPackage generator codebase package@(M.Package Nothing _) =
  M.ScalaSource sourceCode
  where sourceCode       = U.separateNonBlanksWithNewline sourceOfParts <> "}"
        sourceOfParts    = fmap (`R.runReader` J.PackageReaderEnv generator codebase package) parts
        parts            = basicParts

basicParts :: [J.PackageReader M.SourceCode]
basicParts = [ importDeclarations,
               classDeclaration,
               i fieldDeclarations,
               i createDeclaration,
               i fromDeclaration,
               i getters,
               i setters,
               i build ]
             where i = R.mapReader U.indent

importDeclarations :: J.PackageReader M.SourceCode
importDeclarations = do
  return []

packageDeclaration :: J.PackageReader M.SourceCode
packageDeclaration = do
  M.Package (Just packageName) _ <- R.asks J.getPackage
  return $ "package " <> packageName <> ";\n"

importsFromFieldType :: M.FieldType -> J.PackageReader [M.Import]
importsFromFieldType fieldType = do
  needsImport <- J.needsImport fieldType
  if needsImport
    then fmap ((\s -> [s, s <> "Builder"]) =<<) (J.fqns fieldType)
    else case fieldType of
      M.List _              -> return ["java.util.List", "java.util.ArrayList"]
      M.Optional fieldType' -> do
        importsFromFieldType' <- importsFromFieldType fieldType'
        return $ "java.util.Optional" : importsFromFieldType'
      M.Object className    -> return $ case className of
        "LocalDate"                       -> ["java.time.LocalDate"]
        "LocalDateTime"                   -> ["java.time.LocalDateTime"]
        "BigDecimal"                      -> ["java.math.BigDecimal"]
        _                                 -> []
      _                   -> return []

classDeclaration :: J.PackageReader M.SourceCode
classDeclaration = do
  M.Class _ className _ _ _ <- R.asks J.getClass
  return $ "public class " <> className <> "Builder {"

fieldDeclarations :: J.PackageReader M.SourceCode
fieldDeclarations = do
  klass <- R.asks J.getClass
  allFields <- J.getAllFields klass
  declarations <- mapM fieldDeclaration allFields
  return $ concat declarations

fieldDeclaration :: M.Field -> J.PackageReader M.SourceCode
fieldDeclaration (M.Field fieldName fieldType) = do
  domainType <- J.isDomainType fieldType
  return $ "private " <> J.javaize fieldType <>
    (if domainType
     then "Builder " <> fieldName <> " = " <> J.javaize fieldType <> "Builder.create();\n"
     else case fieldType of
       M.Optional _    -> " " <> fieldName <> " = Optional.empty();\n"
       M.List _        -> " " <> fieldName <> " = new ArrayList<>();\n"
       _             -> " " <> fieldName <> ";\n")

getters :: J.PackageReader M.SourceCode
getters = do
  klass <- R.asks J.getClass
  allFields <- J.getAllFields klass
  domainTypeFields <- Monad.filterM (\(M.Field _ fieldType) -> J.isDomainType fieldType) allFields
  return $ L.intercalate "\n" (fmap getter domainTypeFields)

createDeclaration :: J.PackageReader M.SourceCode
createDeclaration = do
  M.Class _ className _ _ _ <- R.asks J.getClass
  return $ "public static " <> className <> "Builder create() " <> block ("return new " <> className <> "Builder();")

fromDeclaration :: J.PackageReader M.SourceCode
fromDeclaration = do
  klass@(M.Class _ className _ _ _) <- R.asks J.getClass
  allFields <- J.getAllFields klass
  return $ "public static " <> className <> "Builder from(" <> className <> " " <> U.downcase className <> ") " <>
    block (
      "return new " <> className <> "Builder()" <>
      (setFieldInFromDeclaration className =<< allFields) <>
      ";"
    )

setFieldInFromDeclaration :: M.ClassName -> M.Field -> M.SourceCode
setFieldInFromDeclaration className (M.Field fieldName fieldType) =
  "." <> "\n" <> ("    set" <> U.upcase fieldName <> "(" <> U.downcase className <> "." <> getOrIs fieldType <> U.upcase fieldName <> "())")

setters :: J.PackageReader M.SourceCode
setters = do
  klass <- R.asks J.getClass
  allFields <- J.getAllFields klass
  setters' <- mapM setter allFields
  return $ L.intercalate "\n" setters'

getter :: M.Field -> M.SourceCode
getter (M.Field fieldName fieldType) =
  "public " <> J.javaize fieldType <> "Builder " <> getOrIs fieldType <> U.upcase fieldName <> "() " <>
  block ("return " <> fieldName <> ";")

setter :: M.Field -> J.PackageReader M.SourceCode
setter field@(M.Field fieldName fieldType) = do
  M.Class _ className _ _ _ <- R.asks J.getClass
  domainType <- J.isDomainType fieldType
  let pb = "public " <> className <> "Builder "
      fn = fieldName
      ft = fieldType
      uf = U.upcase fn
      rt = "return this;"
      rn = requireNonNull fn
    in return . U.separateNonBlanksWithNewline $ case fieldType of
      _ | domainType        -> [
        pb <> "set" <> uf <> "(" <> J.javaize ft <> "Builder " <> fn <> ") " <>
          block (rn <> "this." <> fn <> " = " <> fn <> ";\n" <> rt),
        pb <> "set" <> uf <> "(" <> J.javaize ft <> " " <> fn <> ") " <>
          block (rn <> "this." <> fn <> " = " <> U.upcase (J.javaize ft) <> "Builder.from(" <> fn <> ");\n" <> rt) ]
      M.Optional optionalType -> [
        pb <> "set" <> uf <> "(" <> J.javaize optionalType <> " " <> fn <> ") " <>
          block (rn <> "this." <> fn <> " = Optional.of(" <> fn <> ");\n" <> rt),
        straightFieldSetter className field,
        pb <> "without" <> uf <> "() " <>
          block ("this." <> fn <> " = Optional.empty();\n" <> rt) ]
      M.List memberType       -> [
        pb <> "addTo" <> uf <> "(" <> J.javaize memberType <> " member) " <>
          block (rn <> "this." <> fn <> ".add(member);\n" <> rt),
        straightFieldSetter className field ]
      _                     -> [straightFieldSetter className field]

requireNonNull :: M.FieldName -> M.SourceCode
requireNonNull fieldName =
  "Objects.requireNonNull(" <> fieldName <> ");\n"

straightFieldSetter :: M.ClassName -> M.Field -> M.SourceCode
straightFieldSetter className (M.Field fieldName fieldType) =
  "public " <> className <> "Builder set" <> U.upcase fieldName <> "(" <> J.javaize fieldType <> " " <> fieldName <> ") " <>
  block (
    requireNonNull fieldName <>
    "this." <> fieldName <> " = " <> fieldName <> ";\n" <>
    "return this;"
  )

getOrIs :: M.FieldType -> M.SourceCode
getOrIs fieldType = case fieldType of
  M.Boolean -> "is"
  _       -> "get"

build :: J.PackageReader M.SourceCode
build = do
  klass@(M.Class _ className _ _ _) <- R.asks J.getClass
  allFields <- J.getAllFields klass
  fieldsPart <- mapM buildField allFields
  return $ "public " <> className <> " build() {\n" <>
    U.indent
      (("return new " <> className <> "(\n" <>
      U.indent (L.intercalate ",\n" fieldsPart)
      ) <>
    ");\n") <> "}"

buildField :: M.Field -> J.PackageReader M.SourceCode
buildField (M.Field fieldName fieldType) = do
  domainType <- J.isDomainType fieldType
  return $ "this." <> fieldName <> if domainType then ".build()" else ""

block :: M.SourceCode -> M.SourceCode
block sc = "{\n" <> U.indent sc <> "}\n"

classImports :: J.PackageReader [M.Import]
classImports =
  return [ "java.util.Objects" ]
