module JavaBuilderGenerator (generateJavaClass, importsFromFieldType) where

import Control.Monad
import Control.Monad.Reader
import Data.List (intercalate)

import qualified JavaCommon as J
import qualified Model as M
import qualified Util as U

generateJavaClass :: J.Generator -> M.Codebase -> M.Package -> M.Class -> M.JavaSource
generateJavaClass generator codebase package@(M.Package (Just packageName) _) (M.Class imports className maybeExtends implements fields) =
  M.JavaSource (packageName ++ "." ++ className) sourceCode
  where sourceCode       = U.separateNonBlanksWithNewline sourceOfParts ++ "}"
        sourceOfParts    = map (\p -> runReader p (generator, codebase, package, className, imports, maybeExtends, implements, fields)) parts
        parts            = packageDeclaration : basicParts
generateJavaClass generator codebase package@(M.Package Nothing _) (M.Class imports className maybeExtends implements fields) =
  M.JavaSource className sourceCode
  where sourceCode       = U.separateNonBlanksWithNewline sourceOfParts ++ "}"
        sourceOfParts    = map (\p -> runReader p (generator, codebase, package, className, imports, maybeExtends, implements, fields)) parts
        parts            = basicParts

basicParts :: [J.ClassReader M.SourceCode]
basicParts = [ J.importDeclarations,
               classDeclaration,
               i fieldDeclarations,
               i createDeclaration,
               i fromDeclaration,
               i getters,
               i setters,
               i build ]

i :: J.ClassReader M.SourceCode -> J.ClassReader M.SourceCode
i = mapReader U.indent

packageDeclaration :: J.ClassReader M.SourceCode
packageDeclaration = do
  (_, _, M.Package (Just packageName) _, _, _, _, _, _) <- ask
  return $ "package " ++ packageName ++ ";\n"

importsFromFieldType :: M.FieldType -> J.ClassReader [M.Import]
importsFromFieldType fieldType = do
  needsImport <- J.needsImport fieldType
  if needsImport
    then liftM (concatMap (\s -> [s, s ++ "Builder"])) (J.fqns fieldType)
    else case fieldType of
      M.List _              -> return ["java.util.List", "java.util.ArrayList"]
      M.Optional fieldType' -> do
        importsFromFieldType' <- importsFromFieldType fieldType'
        return $ "java.util.Optional" : importsFromFieldType'
      M.Object className    -> return $ case className of
        "LocalDate"                       -> ["java.time.LocalDate"]
        "LocalDateTime"                   -> ["java.time.LocalDateTime"]
        _                                 -> []
      _                   -> return []

classDeclaration :: J.ClassReader M.SourceCode
classDeclaration = do
  (_, _, _, className, _, _, _, _) <- ask
  return $ "public class " ++ className ++ "Builder {"

fieldDeclarations :: J.ClassReader M.SourceCode
fieldDeclarations = do
  (_, _, _, _, _, _, _, fields) <- ask
  declarations <- mapM fieldDeclaration fields
  return $ concat declarations

fieldDeclaration :: M.Field -> J.ClassReader M.SourceCode
fieldDeclaration (M.Field fieldName fieldType) = do
  domainType <- J.isDomainType fieldType
  return $ "private " ++ J.javaize fieldType ++
    (if domainType
     then "Builder " ++ fieldName ++ " = " ++ J.javaize fieldType ++ "Builder.create();\n"
     else case fieldType of
       M.Optional _    -> " " ++ fieldName ++ " = Optional.empty();\n"
       M.List _        -> " " ++ fieldName ++ " = new ArrayList<>();\n"
       _             -> " " ++ fieldName ++ ";\n")

getters :: J.ClassReader M.SourceCode
getters = do
  (_, _, _, _, _, _, _, fields) <- ask
  domainTypeFields <- filterM (\(M.Field _ fieldType) -> J.isDomainType fieldType) fields
  return $ intercalate "\n" (map getter domainTypeFields)

createDeclaration :: J.ClassReader M.SourceCode
createDeclaration = do
  (_, _, _, className, _, _, _, _) <- ask
  return $ "public static " ++ className ++ "Builder create() " ++ block ("return new " ++ className ++ "Builder();")

fromDeclaration :: J.ClassReader M.SourceCode
fromDeclaration = do
  (_, _, _, className, _, _, _, fields) <- ask
  return $ "public static " ++ className ++ "Builder from(" ++ className ++ " " ++ U.downcase className ++ ") " ++
    block (
      "return new " ++ className ++ "Builder()" ++
      concatMap (setFieldInFromDeclaration className) fields ++
      ";"
    )

setFieldInFromDeclaration :: M.ClassName -> M.Field -> M.SourceCode
setFieldInFromDeclaration className (M.Field fieldName fieldType) =
  "." ++ "\n" ++ ("    set" ++ U.upcase fieldName ++ "(" ++ U.downcase className ++ "." ++ getOrIs fieldType ++ U.upcase fieldName ++ "())")

setters :: J.ClassReader M.SourceCode
setters = do
  (_, _, _, _, _, _, _, fields) <- ask
  setters' <- mapM setter fields
  return $ intercalate "\n" setters'

getter :: M.Field -> M.SourceCode
getter (M.Field fieldName fieldType) =
  "public " ++ J.javaize fieldType ++ "Builder " ++ getOrIs fieldType ++ U.upcase fieldName ++ "() " ++
  block ("return " ++ fieldName ++ ";")

setter :: M.Field -> J.ClassReader M.SourceCode
setter field@(M.Field fieldName fieldType) = do
  (_, _, _, className, _, _, _, _) <- ask
  domainType <- J.isDomainType fieldType
  let pb = "public " ++ className ++ "Builder "
      fn = fieldName
      ft = fieldType
      uf = U.upcase fn
      rt = "return this;"
      rn = requireNonNull fn
    in return $ U.separateNonBlanksWithNewline $ case fieldType of
      _ | domainType        -> [
        pb ++ "set" ++ uf ++ "(" ++ J.javaize ft ++ "Builder " ++ fn ++ ") " ++
          block (rn ++ "this." ++ fn ++ " = " ++ fn ++ ";\n" ++ rt),
        pb ++ "set" ++ uf ++ "(" ++ J.javaize ft ++ " " ++ fn ++ ") " ++
          block (rn ++ "this." ++ fn ++ " = " ++ U.upcase (J.javaize ft) ++ "Builder.from(" ++ fn ++ ");\n" ++ rt) ]
      M.Optional optionalType -> [
        pb ++ "set" ++ uf ++ "(" ++ J.javaize optionalType ++ " " ++ fn ++ ") " ++
          block (rn ++ "this." ++ fn ++ " = Optional.of(" ++ fn ++ ");\n" ++ rt),
        straightFieldSetter className field,
        pb ++ "without" ++ uf ++ "() " ++
          block ("this." ++ fn ++ " = Optional.empty();\n" ++ rt) ]
      M.List memberType       -> [
        pb ++ "addTo" ++ uf ++ "(" ++ J.javaize memberType ++ " member) " ++
          block (rn ++ "this." ++ fn ++ ".add(member);\n" ++ rt),
        straightFieldSetter className field ]
      _                     -> [straightFieldSetter className field]

requireNonNull :: M.FieldName -> M.SourceCode
requireNonNull fieldName =
  "Objects.requireNonNull(" ++ fieldName ++ ");\n"

straightFieldSetter :: M.ClassName -> M.Field -> M.SourceCode
straightFieldSetter className (M.Field fieldName fieldType) =
  "public " ++ className ++ "Builder set" ++ U.upcase fieldName ++ "(" ++ J.javaize fieldType ++ " " ++ fieldName ++ ") " ++
  block (
    requireNonNull fieldName ++
    "this." ++ fieldName ++ " = " ++ fieldName ++ ";\n" ++
    "return this;"
  )

getOrIs :: M.FieldType -> M.SourceCode
getOrIs fieldType = case fieldType of
  M.Boolean -> "is"
  _       -> "get"

build :: J.ClassReader M.SourceCode
build = do
  (_, _, _, className, _, _, _, fields) <- ask
  fieldsPart <- mapM buildField fields
  return $ "public " ++ className ++ " build() {\n" ++
    U.indent
      (("return new " ++ className ++ "(\n" ++
      U.indent (intercalate ",\n" fieldsPart)
      ) ++
    ");\n") ++ "}"

buildField :: M.Field -> J.ClassReader M.SourceCode
buildField (M.Field fieldName fieldType) = do
  domainType <- J.isDomainType fieldType
  return $ "this." ++ fieldName ++ if domainType then ".build()" else ""

block :: M.SourceCode -> M.SourceCode
block sc = "{\n" ++ U.indent sc ++ "}\n"
