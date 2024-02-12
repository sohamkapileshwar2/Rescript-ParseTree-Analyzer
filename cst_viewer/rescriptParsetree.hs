module RescriptParsetree where

import Data.Char

-- Structure Types

type Structure = [StructureItem]

data StructureItem = StructureItem
    { pstrDesc :: StructureItemDesc
    , pstrLoc :: Location
    }

data StructureItemDesc =
    PstrEval Expression Attributes
  | PstrValue RecFlag [ValueBinding]
  | PstrPrimitive ValueDescription
  | PstrType RecFlag [TypeDeclaration]
  | PstrTypext TypeExtension
  | PstrException ExtensionConstructor
  | PstrModule ModuleBinding
  | PstrRecmodule [ModuleBinding]
  | PstrModtype ModuleTypeDeclaration
  | PstrOpen OpenDescription
  | PstrClass ()
  | PstrClassType [ClassTypeDeclaration]
  | PstrInclude IncludeDeclaration
  | PstrAttribute Attribute
  | PstrExtension Extension Attributes

-- Location Types

data Location = Location
    { locStart :: Position
    , locEnd :: Position
    , locGhost :: Bool
    }

data Position = Position
    { posFname :: String
    , posLnum :: Int
    , posBol :: Int
    , posCnum :: Int
    }

type Loc a = 
    { txt :: a
    , loc :: Location
    }

-- Longident Types

data Longident = 
    Lident String
  | Ldot Longident String
  | Lapply Longident Longident

-- Expression Types

data Expression = Expression
    { pexpDesc :: ExpressionDesc
    , pexpLoc :: Location
    , pexpAttributes :: Attributes
    }

data Expression
  = PexpIdent (Loc Longident)
  | PexpConstant Constant
  | PexpLet RecFlag [ValueBinding] Expression
  | PexpFunction [Case]
  | PexpFun ArgLabel (Maybe Expression) Pattern Expression
  | PexpApply Expression [(ArgLabel, Expression)]
  | PexpMatch Expression [Case]
  | PexpTry Expression [Case]
  | PexpTuple [Expression]
  | PexpConstruct (Location Longident) (Maybe Expression)
  | PexpVariant Label (Maybe Expression)
  | PexpRecord [(Location Longident, Expression)] (Maybe Expression)
  | PexpField Expression (Location Longident)
  | PexpSetfield Expression (Location Longident) Expression
  | PexpArray [Expression]
  | PexpIfthenelse Expression Expression (Maybe Expression)
  | PexpSequence Expression Expression
  | PexpWhile Expression Expression
  | PexpFor Pattern Expression Expression DirectionFlag Expression
  | PexpConstraint Expression CoreType
  | PexpCoerce Expression (Maybe CoreType) CoreType
  | PexpSend Expression (Location Label)
  | PexpNew (Location Longident)
  | PexpSetinstvar (Location Label) Expression
  | PexpOverride [(Location Label, Expression)]
  | PexpLetmodule (Location Text) ModuleExpr Expression
  | PexpLetexception ExtensionConstructor Expression
  | PexpAssert Expression
  | PexpLazy Expression
  | PexpPoly Expression (Maybe CoreType)
  | PexpObject ClassStructure
  | PexpNewtype (Location Text) Expression
  | PexpPack ModuleExpr
  | PexpOpen OverrideFlag (Location Longident) Expression
  | PexpExtension Extension
  | PexpUnreachable

-- Attributes Types

type Attributes = [Attribute]

data Attribute = Attribute (Location String) Payload

-- Payload Types

data Payload
  = PStr Structure
  | PSig Signature
  | PTyp CoreType
  | PPat Pattern (Maybe Expression)

-- Signature Types

type Signature = [SignatureItem]

data SignatureItem = SignatureItem
    { psigDesc :: SignatureItemDesc
    , psigLoc :: Location
    }

data SignatureItemDesc = 
    PsigValue ValueDescription
  | PsigType TypeDeclaration
  | PsigTypext TypeExtension
  | PsigException ExtensionConstructor
  | PsigModule ModuleDeclaration
  | PsigRecmodule [ModuleDeclaration]
  | PsigModtype ModuleTypeDeclaration
  | PsigOpen OpenDescription
  | PsigInclude IncludeDeclaration
  | PsigClassType ClassTypeDeclaration
  | PsigAttribute Attribute
  | PsigExtension Extension

-- CoreType Types

data CoreType = CoreType
    { ptypDesc :: CoreTypeDesc
    , ptypLoc :: Location
    , ptypAttributes :: Attributes
    }

data CoreTypeDesc
  = PtypAny
  | PtypVar String
  | PtypArrow ArgLabel CoreType CoreType
  | PtypTuple [CoreType]
  | PtypConstr (Loc Longident) [CoreType]
  | PtypObject [ObjectField] ClosedFlag
  | PtypClass (Loc Longident) [CoreType]
  | PtypAlias CoreType String
  | PtypVariant [RowField] ClosedFlag (Maybe [Label])
  | PtypPoly [Loc String] CoreType
  | PtypPackage PackageType
  | PtypExtension Extension

-- ArgLabel Types

data ArgLabel
  = Nolabel
  | Labelled String
  | Optional String

-- Object Field Types

data ObjectField
  = Otag (Location Label) Attributes CoreType
  | Oinherit CoreType

-- Label

type Label = String

-- Closed Flag Types

data ClosedFlag
  = Closed
  | Open

-- Loc Types

type Loc = Location

-- Row Field Types

data RowField
  = Rtag (Loc Label) Attributes Bool [CoreType]
  | Rinherit CoreType

-- Constant Types

data Constant = 
    PconstInteger String (Maybe Char)
  | PconstChar Int
  | PconstString String (Maybe String)
  | PconstFloat String (Maybe Char)

-- RecFlag Types

data RecFlag = Nonrecursive | Recursive

-- Value Binding Types

data ValueBinding = ValueBinding
    { pvbPat :: Pattern
    , pvbExpr :: Expression
    , pvbAttributes :: Attributes
    , pvbLoc :: Location
    }

-- Pattern Types

data Pattern = Pattern
    { ppatDesc :: PatternDesc
    , ppatLoc :: Location
    , ppatAttributes :: Attributes
    }

data PatternDesc =
    PpatAny
  | PpatVar (Loc String)
  | PpatAlias Pattern (Loc String)
  | PpatConstant Constant
  | PpatInterval Constant Constant
  | PpatTuple [Pattern]
  | PpatConstruct (Loc Longident) (Maybe Pattern)
  | PpatVariant Label (Maybe Pattern)
  | PpatRecord [(Loc Longident, Pattern)] ClosedFlag
  | PpatArray [Pattern]
  | PpatOr Pattern Pattern
  | PpatConstraint Pattern CoreType
  | PpatType (Loc Longident)
  | PpatLazy Pattern
  | PpatUnpack (Loc String)
  | PpatException Pattern
  | PpatExtension Extension
  | PpatOpen (Loc Longident) Pattern

-- Extension Types

type Extension = (Loc Longident) Payload

-- Value Description Types

data ValueDescription = ValueDescription
    { pvalName :: Loc String
    , pvalType :: CoreType
    , pvalPrim :: [String]
    , pvalAttributes :: Attributes
    , pvalLoc :: Location
    }

-- Type Declaration Types

data TypeDeclaration = TypeDeclaration
    { ptypeName :: Loc String
    , ptypeParams :: [(CoreType, Variance)]
    , ptypeCstrs :: [(CoreType, CoreType, Location)]
    , ptypeKind :: TypeKind
    , ptypePrivate :: PrivateFlag
    , ptypeManifest :: Maybe CoreType
    , ptypeAttributes :: Attributes
    , ptypeLoc :: Location
    }

-- Type Extension Types

data TypeExtension = TypeExtension
    { ptyextPath :: Loc Longident
    , ptyextParams :: [(CoreType, Variance)]
    , ptyextConstructors :: [ExtensionConstructor]
    , ptyextPrivate :: PrivateFlag
    , ptyextAttributes :: Attributes
    }

-- TypeKind Types

data TypeKind
  = PtypeAbstract
  | PtypeVariant [ConstructorDeclaration]
  | PtypeRecord [LabelDeclaration]
  | PtypeOpen

-- Constructor Declaration Types

data ConstructorDeclaration = ConstructorDeclaration
    { pcdName :: Loc String
    , pcdArgs :: ConstructorArguments
    , pcdRes :: Maybe CoreType
    , pcdLoc :: Location
    , pcdAttributes :: Attributes
    }

data ConstructorArguments
  = PcstrTuple [CoreType]
  | PcstrRecord [LabelDeclaration]

-- Label Declaration Types

data LabelDeclaration = LabelDeclaration
    { pldName :: Loc String
    , pldMutable :: MutableFlag
    , pldType :: CoreType
    , pldLoc :: Location
    , pldAttributes :: Attributes
    }

-- Mutable Flag Types

data MutableFlag = Immutable | Mutable

-- Variance Types

data Variance
  = Covariant
  | Contravariant
  | Invariant

-- Private Flag Types

data PrivateFlag
  = Private
  | Public








