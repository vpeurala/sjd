module Transform where

import Parser as P
import Model as M

import Data.List (isPrefixOf)
import Data.List.Split (splitOneOf)

transform :: P.CodebaseDeclaration -> M.Codebase
transform (CodebaseDeclaration packageOrClassDeclarations) =
  Codebase $ map declarationToPackage groupedPackages
  where groupedPackages = groupPackages packageOrClassDeclarations

declarationToPackage :: (Maybe P.PackageDeclaration, [P.ClassDeclaration]) -> M.Package
declarationToPackage (Nothing, classDeclarations) =
  M.Package Nothing $ map declarationToClass classDeclarations
declarationToPackage (Just (PackageDeclaration packageName), classDeclarations) =
  M.Package (Just packageName) $ map declarationToClass classDeclarations

groupPackages :: [P.PackageOrClassDeclaration] -> [(Maybe P.PackageDeclaration, [P.ClassDeclaration])]
groupPackages declarations =
  let foldResult = foldl
        (\((currentPackage, currentClasses), packages) declaration -> case declaration of
          (P.OfPackageDeclaration pd) ->
            ((Just pd, []), (currentPackage, currentClasses):packages)
          (P.OfClassDeclaration cd) ->
            ((currentPackage, cd:currentClasses), packages))
        ((Nothing, []), [])
        declarations
  in uncurry (:) foldResult

declarationToClass :: P.ClassDeclaration -> M.Class
declarationToClass (P.ClassDeclaration className extends implements fieldDeclarations) =
  M.Class [] className extends implements (map declarationToField fieldDeclarations)

declarationToField :: P.FieldDeclaration -> M.Field
declarationToField (P.FieldDeclaration fieldName fieldType) =
  M.Field fieldName (declarationToFieldType fieldType)

declarationToFieldType :: String -> M.FieldType
declarationToFieldType f = case f of
  "boolean"                      -> Boolean
  "byte"                         -> Byte
  "short"                        -> Short
  "char"                         -> Char
  "int"                          -> Int
  "long"                         -> Long
  "float"                        -> Float
  "double"                       -> Double
  _ | "List<"     `isPrefixOf` f -> List $ declarationToFieldType $ extractTypeParam f
  _ | "Optional<" `isPrefixOf` f -> Optional $ declarationToFieldType $ extractTypeParam f
  _                              -> Object f

extractTypeParam :: TypeName -> TypeName
extractTypeParam tn = splitOneOf "<>" tn !! 1
