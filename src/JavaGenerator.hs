module JavaGenerator (generateJavaClass, importsFromFieldType, classImports) where

import qualified Control.Monad.Reader as R
import Data.List (intercalate)
import Data.Monoid ((<>))

import qualified JavaCommon as J
import qualified Model as M
import qualified Util as U

generateJavaClass :: J.Generator -> M.Codebase -> M.Package -> M.Class -> M.JavaSource
generateJavaClass generator codebase package@(M.Package (Just packageName) _) klass@(M.Class _ className _ _ _) =
  M.JavaSource (packageName <> "." <> className) sourceCode
  where sourceCode       = U.separateNonBlanksWithNewline sourceOfParts <> "}"
        sourceOfParts    = fmap (`R.runReader` J.ClassReaderEnv generator codebase package klass) parts
        parts            = packageDeclaration : basicParts
generateJavaClass generator codebase package@(M.Package Nothing _) klass@(M.Class _ className _ _ _) =
  M.JavaSource className sourceCode
  where sourceCode       = U.separateNonBlanksWithNewline sourceOfParts <> "}"
        sourceOfParts    = fmap (`R.runReader` J.ClassReaderEnv generator codebase package klass) parts
        parts            = basicParts

basicParts :: [J.ClassReader M.SourceCode]
basicParts = [ J.importDeclarations,
               classDeclaration,
               i fieldDeclarations,
               i constructorDeclaration,
               i getters,
               i toString,
               i equals,
               i hashCode ]
             where i = R.mapReader U.indent

packageDeclaration :: J.ClassReader M.SourceCode
packageDeclaration = do
  M.Package (Just packageName) _ <- R.asks J.getPackage
  return $ "package " <> packageName <> ";\n"

importsFromFieldType :: M.FieldType -> J.ClassReader [M.Import]
importsFromFieldType fieldType = do
  needsImport <- J.needsImport fieldType
  if needsImport
    then J.fqns fieldType
    else case fieldType of
      M.List _              -> return ["java.util.List"]
      M.Optional fieldType' -> do
        importsFromFieldType' <- importsFromFieldType fieldType'
        return $ "java.util.Optional" : importsFromFieldType'
      M.Object className    -> return $ case className of
        "LocalDate"                       -> ["java.time.LocalDate"]
        "LocalDateTime"                   -> ["java.time.LocalDateTime"]
        "BigDecimal"                      -> ["java.math.BigDecimal"]
        _                                 -> []
      _                   -> return []

classDeclaration :: J.ClassReader M.SourceCode
classDeclaration = do
  M.Class _ className extends implements _ <- R.asks J.getClass
  return $ "public class " <> className <> " " <>
    extendsDeclaration extends <>
    implementsDeclaration implements <>
    "{"

extendsDeclaration :: Maybe M.Extends -> M.SourceCode
extendsDeclaration e = case e of
  Nothing  -> ""
  (Just s) -> "extends " <> s <> " "

implementsDeclaration :: M.Implements -> M.SourceCode
implementsDeclaration implements = if null implements
  then ""
  else "implements " <> intercalate ", " implements <> " "

fieldDeclarations :: J.ClassReader M.SourceCode
fieldDeclarations = do
  M.Class _ _ _ _ fields <- R.asks J.getClass
  return $ fieldDeclaration =<< fields

fieldDeclaration :: M.Field -> M.SourceCode
fieldDeclaration (M.Field fieldName fieldType) = "private final " <> J.javaize fieldType <> " " <> fieldName <> ";\n"

constructorDeclaration :: J.ClassReader M.SourceCode
constructorDeclaration = do
  klass@(M.Class _ className _ _ fields) <- R.asks J.getClass
  superClassFields <- J.getSuperClassFields klass
  allFields <- J.getAllFields klass
  return $ "@JsonCreator\n" <>
    "public " <> className <> "(" <> intercalate (",\n" <> replicate (length ("public " <> className <> "(")) ' ') (fmap (\(M.Field fieldName fieldType) -> "@JsonProperty(\"" <> fieldName <> "\") " <> J.javaize fieldType <> " " <> fieldName) allFields) <> ") {\n" <>
    case superClassFields of
      []   -> ""
      scfs -> U.indent $ "super(" <> intercalate ", " (fmap (\(M.Field fieldName _) -> fieldName) scfs) <> ");\n"
    <>
    U.indent ((\(M.Field fieldName _) -> "Objects.requireNonNull(" <> fieldName <> ", \"Property '" <> fieldName <> "' cannot be null.\");\n") =<< fields) <>
    U.indent ((\(M.Field fieldName _) -> "this." <> fieldName <> " = " <> fieldName <> ";\n") =<< fields) <>
    "}\n"

getters :: J.ClassReader M.SourceCode
getters = do
  M.Class _ _ _ _ fields <- R.asks J.getClass
  return $ intercalate "\n" (fmap getter fields)

getter :: M.Field -> M.SourceCode
getter (M.Field fieldName fieldType) = "public " <> J.javaize fieldType <> " " <> getOrIs fieldType <> U.upcase fieldName <> "() {\n" <>
  U.indent ("return " <> fieldName <> ";") <>
  "}\n"

getOrIs :: M.FieldType -> M.SourceCode
getOrIs fieldType = case fieldType of
  M.Boolean -> "is"
  _       -> "get"

toString :: J.ClassReader M.SourceCode
toString = do
  klass@(M.Class _ className _ _ _) <- R.asks J.getClass
  allFields <- J.getAllFields klass
  return $ "@Override\npublic String toString() {\n" <>
    U.indent ("return \"" <> className <> "@\" + System.identityHashCode(this) + \": {\"\n" <>
      U.indent ((\(M.Field fieldName fieldType) -> "+ \"" <> fieldName <> " = '\" + " <> getOrIs fieldType <> U.upcase fieldName <> "() + \"'\"\n") =<< allFields) <>
      ";\n") <>
    "}\n"

equals :: J.ClassReader M.SourceCode
equals = do
  klass@(M.Class _ className _ _ _) <- R.asks J.getClass
  allFields <- J.getAllFields klass
  return $ "@Override\npublic boolean equals(Object o) {\n" <>
    U.indent (
      "if (this == o) return true;\n" <>
      "if (o == null) return false;\n" <>
      "if (this.getClass() != o.getClass()) return false;\n" <>
      className <> " that = (" <> className <> ") o;\n" <>
        ((\(M.Field fieldName fieldType) -> case fieldType of
          M.Boolean                   -> "if (this.is" <> U.upcase fieldName <> "() != that.is" <> U.upcase fieldName <> "()) return false;\n"
          _ | M.isPrimitive fieldType -> "if (this.get" <> U.upcase fieldName <> "() != that.get" <> U.upcase fieldName <> "()) return false;\n"
          _                           -> "if (!this.get" <> U.upcase fieldName <> "().equals(that.get" <> U.upcase fieldName <> "())) return false;\n"
        ) =<< allFields) <>
      "return true;\n"
    ) <>
    "}\n"

hashCode :: J.ClassReader M.SourceCode
hashCode = do
  klass <- R.asks J.getClass
  allFields <- J.getAllFields klass
  return $ "@Override\npublic int hashCode() {\n" <>
    U.indent (
      "int result = 0;\n" <>
        ((\(M.Field fieldName fieldType) -> case fieldType of
          M.Long                      -> "result = 31 * result + Long.hashCode(this.get" <> U.upcase fieldName <> "());\n"
          M.Float                     -> "result = 31 * result + Float.hashCode(this.get" <> U.upcase fieldName <> "());\n"
          M.Double                    -> "result = 31 * result + Double.hashCode(this.get" <> U.upcase fieldName <> "());\n"
          M.Boolean                   -> "result = 31 * result + (this.is" <> U.upcase fieldName <> "() ? 1 : 0);\n"
          _ | M.isPrimitive fieldType -> "result = 31 * result + get" <> U.upcase fieldName <> "();\n"
          _                           -> "result = 31 * result + get" <> U.upcase fieldName <> "().hashCode();\n"
        ) =<< allFields)
      ) <>
    U.indent "return result;\n" <>
    "}\n"

classImports :: J.ClassReader [M.Import]
classImports =
  return [ "java.util.Objects",
           "com.fasterxml.jackson.annotation.JsonCreator",
           "com.fasterxml.jackson.annotation.JsonProperty"]
