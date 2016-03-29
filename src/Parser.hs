module Parser where

import Model

import Text.ParserCombinators.Parsec

import Data.Maybe (fromMaybe)

type FileContent = String

data FieldDeclaration = FieldDeclaration FieldName ClassName deriving (Show)

data PackageOrClassDeclaration = OfPackageDeclaration PackageDeclaration | 
                                 OfClassDeclaration ClassDeclaration deriving (Show)

data PackageDeclaration = PackageDeclaration PackageName deriving (Show)

-- TODO vpeurala 10.11.2015: Support imports
data ClassDeclaration = ClassDeclaration ClassName Implements [FieldDeclaration] deriving (Show)

data CodebaseDeclaration = CodebaseDeclaration [PackageOrClassDeclaration] deriving (Show)

fieldName :: GenParser Char st ClassName
fieldName = many1 (oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"))

className :: GenParser Char st ClassName
className = many1 (oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._"))

typeName :: GenParser Char st TypeName
typeName = many1 (oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._<>"))

packageName :: GenParser Char st PackageName
packageName = many1 (oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "._"))

-- TODO vpeurala 5.11.2015: Take primitives and other special cases into account
fieldType = typeName

fieldDeclaration :: GenParser Char st FieldDeclaration
fieldDeclaration = do
    fieldName' <- fieldName
    spaces
    string ":"
    spaces
    fieldType' <- fieldType
    spaces
    return $ FieldDeclaration fieldName' fieldType'

implements :: GenParser Char st Implements
implements = do
    string "implements "
    spaces
    types <- many1 typeName
    spaces
    return types

classDeclaration :: GenParser Char st PackageOrClassDeclaration
classDeclaration = do
    string "class "
    spaces
    className' <- className
    spaces
    implements' <- optionMaybe implements
    let implements'' = fromMaybe [] implements'
    spaces
    fields <- many (try fieldDeclaration)
    spaces
    return $ OfClassDeclaration $ ClassDeclaration className' implements'' fields

packageDeclaration :: GenParser Char st PackageOrClassDeclaration
packageDeclaration = do
    string "package "
    spaces
    packageName' <- packageName
    spaces
    return $ OfPackageDeclaration $ PackageDeclaration packageName'

packageOrClassDeclaration :: GenParser Char st PackageOrClassDeclaration
packageOrClassDeclaration =
    try packageDeclaration <|> classDeclaration

codebase :: GenParser Char st CodebaseDeclaration
codebase = do
    packageOrClassDeclarations <- many packageOrClassDeclaration
    eof
    return $ CodebaseDeclaration packageOrClassDeclarations

parseCodebase :: FileContent -> Either ParseError CodebaseDeclaration
parseCodebase = parse codebase "TODO: FILENAME"