module JavaBuilderGenerator (generateJavaClass, importsFromFieldType) where

import Control.Monad
import Control.Monad.Reader
import Data.List (intercalate)

import qualified JavaCommon as J
import Model as M
import Util as U

generateJavaClass :: J.Generator -> M.Codebase -> M.Package -> M.Class -> JavaSource
generateJavaClass generator codebase package@(M.Package (Just packageName) _) (M.Class imports className maybeExtends implements fields) =
  JavaSource (packageName ++ "." ++ className) sourceCode
  where sourceCode       = separateNonBlanksWithNewline parts ++ "}"
        parts            = [ packageDeclaration packageName,
                             runReader (J.importDeclarations imports maybeExtends implements fields) (generator, codebase, package, className),
                             classDeclaration className maybeExtends implements,
                             partWithReader (fieldDeclarations fields),
                             partWithReader createDeclaration,
                             partWithReader (fromDeclaration fields),
                             partWithReader (getters fields),
                             partWithReader (setters fields),
                             partWithReader (build fields) ]
        partWithReader p = indent (runReader p (generator, codebase, package, className))
generateJavaClass generator codebase package@(M.Package Nothing _) (M.Class imports className maybeExtends implements fields) =
  JavaSource className sourceCode
  where sourceCode       = separateNonBlanksWithNewline parts ++ "}"
        parts            = [ runReader (J.importDeclarations imports maybeExtends implements fields) (generator, codebase, package, className),
                             classDeclaration className maybeExtends implements,
                             partWithReader (fieldDeclarations fields),
                             partWithReader createDeclaration,
                             partWithReader (fromDeclaration fields),
                             partWithReader (getters fields),
                             partWithReader (setters fields),
                             partWithReader (build fields) ]
        partWithReader p = indent (runReader p (generator, codebase, package, className))

packageDeclaration :: PackageName -> SourceCode
packageDeclaration packageName = "package " ++ packageName ++ ";\n"

importsFromFieldType :: M.FieldType -> J.ClassReader [Import]
importsFromFieldType fieldType = do
  needsImport <- J.needsImport fieldType
  if needsImport
    then liftM (concatMap (\s -> [s, s ++ "Builder"])) (J.fqns fieldType)
    else case fieldType of
      List _              -> return ["java.util.List", "java.util.ArrayList"]
      Optional fieldType' -> do
        importsFromFieldType' <- importsFromFieldType fieldType'
        return $ "java.util.Optional" : importsFromFieldType'
      Object className    -> return $ case className of
        "LocalDate"                       -> ["java.time.LocalDate"]
        "LocalDateTime"                   -> ["java.time.LocalDateTime"]
        _                                 -> []
      _                   -> return []

classDeclaration :: ClassName -> Maybe Extends -> Implements -> SourceCode
classDeclaration className _ _ = "public class " ++ className ++ "Builder {"

fieldDeclarations :: [M.Field] -> J.ClassReader SourceCode
fieldDeclarations fields = do
  declarations <- mapM fieldDeclaration fields
  return $ concat declarations

fieldDeclaration :: M.Field -> J.ClassReader SourceCode
fieldDeclaration (M.Field fieldName fieldType) = do
  domainType <- J.isDomainType fieldType
  return $ "private " ++ J.javaize fieldType ++
    (if domainType
     then "Builder " ++ fieldName ++ " = " ++ J.javaize fieldType ++ "Builder.create();\n"
     else case fieldType of
       Optional _    -> " " ++ fieldName ++ " = Optional.empty();\n"
       List _        -> " " ++ fieldName ++ " = new ArrayList<>();\n"
       _             -> " " ++ fieldName ++ ";\n")

getters :: [M.Field] -> J.ClassReader SourceCode
getters fields = do
  domainTypeFields <- filterM (\(M.Field _ fieldType) -> J.isDomainType fieldType) fields
  return $ intercalate "\n" (map getter domainTypeFields)

createDeclaration :: J.ClassReader SourceCode
createDeclaration = do
  (_, _, _, className) <- ask
  return $ "public static " ++ className ++ "Builder create() " ++ block ("return new " ++ className ++ "Builder();")

fromDeclaration :: [M.Field] -> J.ClassReader SourceCode
fromDeclaration fields = do
  (_, _, _, className) <- ask
  return $ "public static " ++ className ++ "Builder from(" ++ className ++ " " ++ downcase className ++ ") " ++
    block (
      "return new " ++ className ++ "Builder()" ++
      concatMap (setFieldInFromDeclaration className) fields ++
      ";"
    )

setFieldInFromDeclaration :: ClassName -> M.Field -> SourceCode
setFieldInFromDeclaration className (M.Field fieldName fieldType) =
  "." ++ "\n" ++ ("    set" ++ upcase fieldName ++ "(" ++ downcase className ++ "." ++ getOrIs fieldType ++ upcase fieldName ++ "())")

setters :: [M.Field] -> J.ClassReader SourceCode
setters fields = liftM (intercalate "\n") (mapM setter fields)

getter :: M.Field -> SourceCode
getter (M.Field fieldName fieldType) =
  "public " ++ J.javaize fieldType ++ "Builder " ++ getOrIs fieldType ++ upcase fieldName ++ "() " ++
  block ("return " ++ fieldName ++ ";")

setter :: M.Field -> J.ClassReader SourceCode
setter field@(M.Field fieldName fieldType) = do
  (_, _, _, className) <- ask
  domainType <- J.isDomainType fieldType
  let pb = "public " ++ className ++ "Builder "
      fn = fieldName
      ft = fieldType
      uf = upcase fn
      rt = "return this;"
      rn = requireNonNull fn
    in return $ separateNonBlanksWithNewline $ case fieldType of
      _ | domainType        -> [
        pb ++ "set" ++ uf ++ "(" ++ J.javaize ft ++ "Builder " ++ fn ++ ") " ++
          block (rn ++ "this." ++ fn ++ " = " ++ fn ++ ";\n" ++ rt),
        pb ++ "set" ++ uf ++ "(" ++ J.javaize ft ++ " " ++ fn ++ ") " ++
          block (rn ++ "this." ++ fn ++ " = " ++ upcase (J.javaize ft) ++ "Builder.from(" ++ fn ++ ");\n" ++ rt) ]
      Optional optionalType -> [
        pb ++ "set" ++ uf ++ "(" ++ J.javaize optionalType ++ " " ++ fn ++ ") " ++
          block (rn ++ "this." ++ fn ++ " = Optional.of(" ++ fn ++ ");\n" ++ rt),
        straightFieldSetter className field,
        pb ++ "without" ++ uf ++ "() " ++
          block ("this." ++ fn ++ " = Optional.empty();\n" ++ rt) ]
      List memberType       -> [
        pb ++ "addTo" ++ uf ++ "(" ++ J.javaize memberType ++ " member) " ++
          block (rn ++ "this." ++ fn ++ ".add(member);\n" ++ rt),
        straightFieldSetter className field ]
      _                     -> [straightFieldSetter className field]

requireNonNull :: FieldName -> SourceCode
requireNonNull fieldName =
  "Objects.requireNonNull(" ++ fieldName ++ ");\n"

straightFieldSetter :: ClassName -> Field -> SourceCode
straightFieldSetter className (M.Field fieldName fieldType) =
  "public " ++ className ++ "Builder set" ++ upcase fieldName ++ "(" ++ J.javaize fieldType ++ " " ++ fieldName ++ ") " ++
  block (
    requireNonNull fieldName ++
    "this." ++ fieldName ++ " = " ++ fieldName ++ ";\n" ++
    "return this;"
  )

getOrIs :: M.FieldType -> SourceCode
getOrIs fieldType = case fieldType of
  Boolean -> "is"
  _       -> "get"

build :: [M.Field] -> J.ClassReader SourceCode
build fields = do
  (_, _, _, className) <- ask
  fieldsPart <- mapM buildField fields
  return $ "public " ++ className ++ " build() {\n" ++
    indent
      (("return new " ++ className ++ "(\n" ++
      indent (intercalate ",\n" fieldsPart)
      ) ++
    ");\n") ++ "}"

buildField :: M.Field -> J.ClassReader SourceCode
buildField (M.Field fieldName fieldType) = do
  domainType <- J.isDomainType fieldType
  return $ "this." ++ fieldName ++ if domainType then ".build()" else ""

block :: SourceCode -> SourceCode
block sc = "{\n" ++ indent sc ++ "}\n"
