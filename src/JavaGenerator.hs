module JavaGenerator (generateJavaClass, importsFromFieldType, classImports) where

import Control.Monad.Reader
import Data.List (intercalate)

import qualified JavaCommon as J
import qualified Model as M
import qualified Util as U

generateJavaClass :: J.Generator -> M.Codebase -> M.Package -> M.Class -> M.JavaSource
generateJavaClass generator codebase package@(M.Package (Just packageName) _) klass@(M.Class _ className maybeExtends implements fields) =
  M.JavaSource (packageName ++ "." ++ className) sourceCode
  where sourceCode = packageDeclaration packageName ++
                     runReader J.importDeclarations (J.ClassReaderEnv generator codebase package klass) ++
                     "\n" ++
                     classDeclaration className maybeExtends implements ++
                     U.indent (fieldDeclarations fields) ++
                     "\n" ++
                     U.indent (constructorDeclaration className fields) ++
                     U.indent (getters fields) ++
                     "\n" ++
                     U.indent (toString className fields) ++
                     "\n" ++
                     U.indent (equals className fields) ++
                     "\n" ++
                     U.indent (hashCode fields) ++
                     "}"
generateJavaClass generator codebase package@(M.Package Nothing _) klass@(M.Class _ className maybeExtends implements fields) =
  M.JavaSource className sourceCode
  where sourceCode = runReader J.importDeclarations (J.ClassReaderEnv generator codebase package klass) ++
                     "\n" ++
                     classDeclaration className maybeExtends implements ++
                     U.indent (fieldDeclarations fields) ++
                     "\n" ++
                     U.indent (constructorDeclaration className fields) ++
                     U.indent (getters fields) ++
                     "\n" ++
                     U.indent (toString className fields) ++
                     "\n" ++
                     U.indent (equals className fields) ++
                     "\n" ++
                     U.indent (hashCode fields) ++
                     "}"

packageDeclaration :: M.PackageName -> M.SourceCode
packageDeclaration packageName = "package " ++ packageName ++ ";\n\n"

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
        _                                 -> []
      _                   -> return []

classDeclaration :: M.ClassName -> Maybe M.Extends -> M.Implements -> M.SourceCode
classDeclaration className extends implements = "public class " ++ className ++ " " ++
  extendsDeclaration extends ++
  implementsDeclaration implements ++
  "{\n"

extendsDeclaration :: Maybe M.Extends -> M.SourceCode
extendsDeclaration e = case e of
  Nothing  -> ""
  (Just s) -> "extends " ++ s ++ " "

implementsDeclaration :: M.Implements -> M.SourceCode
implementsDeclaration implements = if null implements
  then ""
  else "implements " ++ intercalate ", " implements ++ " "

fieldDeclarations :: [M.Field] -> M.SourceCode
fieldDeclarations = concatMap fieldDeclaration

fieldDeclaration :: M.Field -> M.SourceCode
fieldDeclaration (M.Field fieldName fieldType) = "private final " ++ J.javaize fieldType ++ " " ++ fieldName ++ ";\n"

constructorDeclaration :: M.ClassName -> [M.Field] -> M.SourceCode
constructorDeclaration className fields =
  "@JsonCreator\n" ++
  "public " ++ className ++ "(" ++ intercalate (",\n" ++ replicate (length ("public " ++ className ++ "(")) ' ') (map (\(M.Field fieldName fieldType) -> "@JsonProperty(\"" ++ fieldName ++ "\") " ++ J.javaize fieldType ++ " " ++ fieldName) fields) ++ ") {\n" ++
  U.indent (concatMap (\(M.Field fieldName _) -> "Objects.requireNonNull(" ++ fieldName ++ ", \"Property '" ++ fieldName ++ "' cannot be null.\");\n") fields) ++
  U.indent (concatMap (\(M.Field fieldName _) -> "this." ++ fieldName ++ " = " ++ fieldName ++ ";\n") fields) ++
  "}\n\n"

getters :: [M.Field] -> M.SourceCode
getters fields = intercalate "\n" (map getter fields)

getter :: M.Field -> M.SourceCode
getter (M.Field fieldName fieldType) = "public " ++ J.javaize fieldType ++ " " ++ getOrIs fieldType ++ U.upcase fieldName ++ "() {\n" ++
  U.indent ("return " ++ fieldName ++ ";") ++
  "}\n"

getOrIs :: M.FieldType -> M.SourceCode
getOrIs fieldType = case fieldType of
  M.Boolean -> "is"
  _       -> "get"

toString :: M.ClassName -> [M.Field] -> M.SourceCode
toString className fields = "@Override\npublic String toString() {\n" ++
  U.indent ("return \"" ++ className ++ "@\" + System.identityHashCode(this) + \": {\"\n" ++
    U.indent (concatMap (\(M.Field fieldName _) -> "+ \"" ++ fieldName ++ " = '\" + " ++ fieldName ++ " + \"'\"\n") fields) ++
    ";\n") ++
  "}\n"

equals :: M.ClassName -> [M.Field] -> M.SourceCode
equals className fields = "@Override\npublic boolean equals(Object o) {\n" ++
  U.indent (
    "if (this == o) return true;\n" ++
    "if (o == null) return false;\n" ++
    "if (this.getClass() != o.getClass()) return false;\n" ++
    className ++ " that = (" ++ className ++ ") o;\n" ++
    concatMap (
      \(M.Field fieldName fieldType) ->
        if M.isPrimitive fieldType then
          "if (this." ++ fieldName ++ " != that." ++ fieldName ++ ") return false;\n"
        else
          "if (!this." ++ fieldName ++ ".equals(that." ++ fieldName ++ ")) return false;\n"
      ) fields ++
    "return true;\n"
  ) ++
  "}\n"

hashCode :: [M.Field] -> M.SourceCode
hashCode fields = "@Override\npublic int hashCode() {\n" ++
  U.indent (
    "int result = 0;\n" ++
    concatMap (
      \(M.Field fieldName fieldType) -> case fieldType of
        M.Long                        -> "result = 31 * Long.hashCode(" ++ fieldName ++ ");\n"
        M.Float                       -> "result = 31 * Float.hashCode(" ++ fieldName ++ ");\n"
        M.Double                      -> "result = 31 * Double.hashCode(" ++ fieldName ++ ");\n"
        M.Boolean                     -> "result = 31 * result + (" ++ fieldName ++ " ? 1 : 0);\n"
        _ | M.isPrimitive fieldType -> "result = 31 * result + " ++ fieldName ++ ";\n"
        _                           -> "result = 31 * result + " ++ fieldName ++ ".hashCode();\n"
      ) fields
    ) ++
  U.indent "return result;\n" ++
  "}\n"

classImports :: J.ClassReader [M.Import]
classImports =
  return [ "java.util.Objects",
           "com.fasterxml.jackson.annotation.JsonCreator",
           "com.fasterxml.jackson.annotation.JsonProperty"]
