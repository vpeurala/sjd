module Transform where

import qualified Parser as P
import qualified Model as M

import Data.List (isPrefixOf)
import Data.List.Split (splitOneOf)

transform :: P.CodebaseDeclaration -> M.Codebase
transform (P.CodebaseDeclaration packageOrClassDeclarations) =
  M.Codebase $ fmap declarationToPackage groupedPackages
  where groupedPackages = groupPackages packageOrClassDeclarations

declarationToPackage :: (Maybe P.PackageDeclaration, [P.ClassDeclaration]) -> M.Package
declarationToPackage (Nothing, classDeclarations) =
  M.Package Nothing $ fmap declarationToClass classDeclarations
declarationToPackage (Just (P.PackageDeclaration packageName), classDeclarations) =
  M.Package (Just packageName) $ fmap declarationToClass classDeclarations

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
  M.Class [] className extends implements (fmap declarationToField fieldDeclarations)

declarationToField :: P.FieldDeclaration -> M.Field
declarationToField (P.FieldDeclaration fieldName fieldType) =
  M.Field fieldName (declarationToFieldType fieldType)

declarationToFieldType :: String -> M.FieldType
declarationToFieldType f = case f of
  "boolean"                      -> M.Boolean
  "byte"                         -> M.Byte
  "short"                        -> M.Short
  "char"                         -> M.Char
  "int"                          -> M.Int
  "long"                         -> M.Long
  "float"                        -> M.Float
  "double"                       -> M.Double
  _ | "List<"     `isPrefixOf` f -> M.List . declarationToFieldType $ extractTypeParam f
  _ | "Optional<" `isPrefixOf` f -> M.Optional . declarationToFieldType $ extractTypeParam f
  _                              -> M.Object f

extractTypeParam :: M.TypeName -> M.TypeName
extractTypeParam tn = splitOneOf "<>" tn !! 1
