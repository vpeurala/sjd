module Model where

type ClassName = String

type FullyQualifiedClassName = String

type PackageName = String

type TypeName = String

type Extends = String

type Implements = [TypeName]

type FieldName = String

type Import = String

data FieldType = Boolean |
                 Byte |
                 Short |
                 Char |
                 Int |
                 Long |
                 Float |
                 Double |
                 List FieldType |
                 Optional FieldType |
                 Object ClassName
                 deriving (Eq, Ord, Show)

data Field = Field FieldName FieldType deriving (Eq, Ord, Show)

data Class = Class [Import] ClassName (Maybe Extends) Implements [Field] deriving (Eq, Ord, Show)

data Package = Package (Maybe PackageName) [Class] deriving (Eq, Ord, Show)

data Codebase = Codebase [Package] deriving (Eq, Ord, Show)

type SourceCode = String

data JavaSource = JavaSource FullyQualifiedClassName SourceCode deriving (Eq, Ord, Show)

isPrimitive :: FieldType -> Bool
isPrimitive ft = case ft of
  Boolean -> True
  Byte    -> True
  Short   -> True
  Char    -> True
  Int     -> True
  Long    -> True
  Float   -> True
  Double  -> True
  _       -> False
