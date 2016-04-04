module JavaGenerator (generateJavaClass, importsFromFieldType) where

import Control.Monad.Reader
import Data.Char (toUpper)
import Data.List (intercalate, intersperse, isPrefixOf, nub)
import Data.Maybe (catMaybes)

import qualified JavaCommon as J
import Model as M
import Util as U

generateJavaClass :: J.Generator -> M.Codebase -> M.Package -> M.Class -> JavaSource
generateJavaClass generator codebase package@(M.Package (Just packageName) _) (M.Class imports className maybeExtends implements fields) =
  JavaSource (packageName ++ "." ++ className) sourceCode
  where sourceCode = packageDeclaration packageName ++
                     runReader J.importDeclarations (generator, codebase, package, className, imports, maybeExtends, implements, fields) ++
                     "\n" ++
                     classDeclaration className maybeExtends implements ++
                     indent (fieldDeclarations fields) ++
                     "\n" ++
                     indent (constructorDeclaration className fields) ++
                     indent (getters fields) ++
                     "\n" ++
                     indent (toString className fields) ++
                     "\n" ++
                     indent (equals className fields) ++
                     "\n" ++
                     indent (hashCode fields) ++
                     "}"
generateJavaClass generator codebase package@(M.Package Nothing _) (M.Class imports className maybeExtends implements fields) =
  JavaSource className sourceCode
  where sourceCode = runReader J.importDeclarations (generator, codebase, package, className, imports, maybeExtends, implements, fields) ++
                     "\n" ++
                     classDeclaration className maybeExtends implements ++
                     indent (fieldDeclarations fields) ++
                     "\n" ++
                     indent (constructorDeclaration className fields) ++
                     indent (getters fields) ++
                     "\n" ++
                     indent (toString className fields) ++
                     "\n" ++
                     indent (equals className fields) ++
                     "\n" ++
                     indent (hashCode fields) ++
                     "}"

packageDeclaration :: PackageName -> SourceCode
packageDeclaration packageName = "package " ++ packageName ++ ";\n\n"

importsFromFieldType :: M.FieldType -> J.ClassReader [Import]
importsFromFieldType fieldType = do
  needsImport <- J.needsImport fieldType
  if needsImport
    then J.fqns fieldType
    else case fieldType of
      List fieldType'     -> return ["java.util.List"]
      Optional fieldType' -> do
        importsFromFieldType' <- importsFromFieldType fieldType'
        return $ "java.util.Optional" : importsFromFieldType'
      Object className    -> return $ case className of
        "LocalDate"                       -> ["java.time.LocalDate"]
        "LocalDateTime"                   -> ["java.time.LocalDateTime"]
        _                                 -> []
      _                   -> return []

classDeclaration :: ClassName -> Maybe Extends -> Implements -> SourceCode
classDeclaration className extends implements = "public class " ++ className ++ " " ++
  extendsDeclaration extends ++
  implementsDeclaration implements ++
  "{\n"

extendsDeclaration :: Maybe Extends -> SourceCode
extendsDeclaration e = case e of
  Nothing  -> ""
  (Just s) -> "extends " ++ s ++ " "

implementsDeclaration :: Implements -> SourceCode
implementsDeclaration implements = if null implements
  then ""
  else "implements " ++ intercalate ", " implements ++ " "

fieldDeclarations :: [M.Field] -> SourceCode
fieldDeclarations = concatMap fieldDeclaration

fieldDeclaration :: M.Field -> SourceCode
fieldDeclaration (M.Field fieldName fieldType) = "private final " ++ J.javaize fieldType ++ " " ++ fieldName ++ ";\n"

constructorDeclaration :: ClassName -> [M.Field] -> SourceCode
constructorDeclaration className fields =
  "@JsonCreator\n" ++
  "public " ++ className ++ "(" ++ intercalate (",\n" ++ (replicate (length ("public " ++ className ++ "(")) ' ')) (map (\(M.Field fieldName fieldType) -> "@JsonProperty(\"" ++ fieldName ++ "\") " ++ J.javaize fieldType ++ " " ++ fieldName) fields) ++ ") {\n" ++
  indent (concatMap (\(M.Field fieldName _) -> "Objects.requireNonNull(" ++ fieldName ++ ", \"Property '" ++ fieldName ++ "' cannot be null.\");\n") fields) ++
  indent (concatMap (\(M.Field fieldName _) -> "this." ++ fieldName ++ " = " ++ fieldName ++ ";\n") fields) ++
  "}\n\n"

getters :: [M.Field] -> SourceCode
getters fields = intercalate "\n" (map getter fields)

getter :: M.Field -> SourceCode
getter (M.Field fieldName fieldType) = "public " ++ J.javaize fieldType ++ " " ++ getOrIs fieldType ++ upcase fieldName ++ "() {\n" ++
  indent ("return " ++ fieldName ++ ";") ++
  "}\n"

getOrIs :: M.FieldType -> SourceCode
getOrIs fieldType = case fieldType of
  Boolean -> "is"
  _       -> "get"

toString :: ClassName -> [M.Field] -> SourceCode
toString className fields = "@Override\npublic String toString() {\n" ++
  indent ("return \"" ++ className ++ "@\" + System.identityHashCode(this) + \": {\"\n" ++
    indent (concatMap (\(M.Field fieldName _) -> "+ \"" ++ fieldName ++ " = '\" + " ++ fieldName ++ " + \"'\"\n") fields) ++
    ";\n") ++
  "}\n"

equals :: ClassName -> [M.Field] -> SourceCode
equals className fields = "@Override\npublic boolean equals(Object o) {\n" ++
  indent (
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

hashCode :: [M.Field] -> SourceCode
hashCode fields = "@Override\npublic int hashCode() {\n" ++
  indent (
    "int result = 0;\n" ++
    concatMap (
      \(M.Field fieldName fieldType) -> case fieldType of
        Long                        -> "result = 31 * Long.hashCode(" ++ fieldName ++ ");\n"
        Float                       -> "result = 31 * Float.hashCode(" ++ fieldName ++ ");\n"
        Double                      -> "result = 31 * Double.hashCode(" ++ fieldName ++ ");\n"
        Boolean                     -> "result = 31 * result + (" ++ fieldName ++ " ? 1 : 0);\n"
        _ | M.isPrimitive fieldType -> "result = 31 * result + " ++ fieldName ++ ";\n"
        _                           -> "result = 31 * result + " ++ fieldName ++ ".hashCode();\n"
      ) fields
    ) ++
  indent "return result;\n" ++
  "}\n"
