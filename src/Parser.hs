{-# LANGUAGE RankNTypes #-}
module Parser where

import Model
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Data.Maybe (fromMaybe)

type FileContent = String
type FileName = String

data FieldDeclaration = FieldDeclaration FieldName ClassName deriving (Show)

data TopLevelDeclaration = OfPackageDeclaration PackageDeclaration |
                           OfClassDeclaration ClassDeclaration |
                           OfUnionDeclaration UnionDeclaration deriving (Show)

data PackageDeclaration = PackageDeclaration PackageName deriving (Show)

-- TODO vpeurala 10.11.2015: Support imports
data ClassDeclaration = ClassDeclaration ClassName (Maybe Extends) Implements [FieldDeclaration] deriving (Show)

data UnionDeclaration = UnionDeclaration ClassName deriving (Show)

data CodebaseDeclaration = CodebaseDeclaration [TopLevelDeclaration] deriving (Show)

comment = do
  P.string "#" >> P.manyTill P.anyChar P.newline >> P.spaces

spaces = (P.try (P.many comment) >> P.spaces)

fieldName = P.many1 . P.oneOf $ mconcat [['A'..'Z'], ['a'..'z'], ['0'..'9'], "_"]

className = P.many1 . P.oneOf $ mconcat [['A'..'Z'], ['a'..'z'], ['0'..'9'], "._"]

typeName = P.many1 . P.oneOf $ mconcat [['A'..'Z'], ['a'..'z'], ['0'..'9'], "._<>"]

packageName = P.many1 . P.oneOf $ mconcat [['A'..'Z'], ['a'..'z'], ['0'..'9'], "._"]

-- TODO vpeurala 5.11.2015: Take primitives and other special cases into account
fieldType = typeName

constructorDeclaration = do
    P.manyTill P.anyChar P.newline
    return ()

fieldDeclaration = do
    fieldName' <- fieldName
    spaces
    _ <- P.string ":"
    spaces
    fieldType' <- fieldType
    spaces
    return $ FieldDeclaration fieldName' fieldType'

extends = do
    _ <- P.try $ P.string "extends "
    spaces
    type' <- typeName
    spaces
    return type'

implements = do
    _ <- P.try $ P.string "implements "
    spaces
    types <- P.many1 typeName
    spaces
    return types

classDeclaration = do
    _ <- P.string "class "
    spaces
    className' <- className
    spaces
    extends' <- P.optionMaybe extends
    implements' <- P.optionMaybe implements
    let implements'' = fromMaybe [] implements'
    spaces
    fields <- P.many (P.try fieldDeclaration)
    spaces
    return . OfClassDeclaration $ ClassDeclaration className' extends' implements'' fields

packageDeclaration = do
    _ <- P.string "package "
    spaces
    packageName' <- packageName
    spaces
    return . OfPackageDeclaration $ PackageDeclaration packageName'

unionDeclaration = do
    _ <- P.string "union "
    spaces
    unionName' <- className
    spaces
    constructors <- P.many (P.try constructorDeclaration)
    return . OfUnionDeclaration $ UnionDeclaration unionName'

topLevelDeclaration = do
    spaces
    pocD <- P.try packageDeclaration
            <|> P.try classDeclaration
            <|> unionDeclaration
    spaces
    return pocD

codebase = do
    spaces
    packageOrClassDeclarations <- P.sepBy topLevelDeclaration spaces
    P.eof
    return $ CodebaseDeclaration packageOrClassDeclarations

parseCodebase :: FileName -> FileContent -> Either P.ParseError CodebaseDeclaration
parseCodebase = P.parse codebase
