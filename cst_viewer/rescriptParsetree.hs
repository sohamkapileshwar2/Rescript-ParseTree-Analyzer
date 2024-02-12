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

-- Longident Types

data Longident =
    Lident String
  | Ldot Longident String
  | Lapply Longident Longident

-- Module Types

data ModuleBinding = ModuleBinding
    { pmbName :: Loc String
    , pmbExpr :: ModuleExpr
    , pmbAttributes :: Attributes
    , pmbLoc :: Location
    }

data ModuleExpr = ModuleExpr
    { pmodDesc :: ModuleExprDesc
    , pmodLoc :: Location
    , pmodAttributes :: Attributes
    }

data ModuleExprDesc
    = PmodIdent (Loc Longident)
    | PmodStructure Structure
    | PmodFunctor (Loc String) (Maybe ModuleType) ModuleExpr
    | PmodApply ModuleExpr ModuleExpr
    | PmodConstraint ModuleExpr ModuleType
    | PmodUnpack Expression
    | PmodExtension Extension

data ModuleType = ModuleType
    { pmtyDesc :: ModuleTypeDesc
    , pmtyLoc :: Location
    , pmtyAttributes :: Attributes
    }

data ModuleTypeDesc
    = PmtyIdent (Loc Longident)
    | PmtySignature Signature
    | PmtyFunctor (Loc String) (Maybe ModuleType) ModuleType
    | PmtyWith ModuleType [WithConstraint]
    | PmtyTypeof ModuleExpr
    | PmtyExtension Extension
    | PmtyAlias (Loc Longident)

data ModuleDeclaration = ModuleDeclaration
    { pmdName :: Loc String
    , pmdType :: ModuleType
    , pmdAttributes :: Attributes
    , pmdLoc :: Location
    }

data ModuleTypeDeclaration = ModuleTypeDeclaration
    { pmtdName :: Loc String
    , pmtdType :: Maybe ModuleType
    , pmtdAttributes :: Attributes
    , pmtdLoc :: Location
    }

-- Open Description

data OpenDescription = OpenDescription
    { popenLid :: Loc Longident
    , popenOverride :: OverrideFlag
    , popenLoc :: Location
    , popenAttributes :: Attributes
    }

-- Include Description

data IncludeDescription = IncludeDescription IncludeInfos

data IncludeInfos a = IncludeInfos
    { pinclMod :: a
    , pinclLoc :: Location
    , pinclAttributes :: Attributes
    }

data IncludeDeclaration = IncludeDeclaration (ModuleExpr (IncludeInfos ModuleExpr))

-- With Constraint

data WithConstraint
    = PwithType (Loc Longident) TypeDeclaration
    | PwithModule (Loc Longident) (Loc Longident)
    | PwithTypeSubst (Loc Longident) TypeDeclaration
    | PwithModSubst (Loc Longident) (Loc Longident)


-- Class Types

type ClassTypeDeclaration = ClassType ClassInfos

data ClassInfos a = ClassInfos
    { pciVirt :: VirtualFlag
    , pciParams :: [(CoreType, Variance)]
    , pciName :: Loc String
    , pciExpr :: a
    , pciLoc :: Location
    , pciAttributes :: Attributes
    }

data ClassStructure = ClassStructure
    { pcstrSelf :: Pattern
    , pcstrFields :: [ClassField]
    }

data ClassField = ClassField
    { pcfDesc :: ClassFieldDesc
    , pcfLoc :: Location
    , pcfAttributes :: Attributes
    }

data ClassFieldDesc
    = PcfInherit ()
    | PcfVal (Loc Label) MutableFlag ClassFieldKind
    | PcfMethod (Loc Label) PrivateFlag ClassFieldKind
    | PcfConstraint (CoreType, CoreType)
    | PcfInitializer Expression
    | PcfAttribute Attribute
    | PcfExtension Extension

data ClassFieldKind
    = CfkVirtual CoreType
    | CfkConcrete OverrideFlag Expression

data ClassType = ClassType
    { pctyDesc :: ClassTypeDesc
    , pctyLoc :: Location
    , pctyAttributes :: Attributes
    }

data ClassTypeDesc
    = PctyConstr (Loc Longident) [CoreType]
    | PctySignature ClassSignature
    | PctyArrow ArgLabel CoreType ClassType
    | PctyExtension Extension
    | PctyOpen OverrideFlag (Loc Longident) ClassType

data ClassSignature = ClassSignature
    { pcsigSelf :: CoreType
    , pcsigFields :: [ClassTypeField]
    }

data ClassTypeField = ClassTypeField
    { pctfDesc :: ClassTypeFieldDesc
    , pctfLoc :: Location
    , pctfAttributes :: Attributes
    }

data ClassTypeFieldDesc
    = PctfInherit ClassType
    | PctfVal (Loc Label) MutableFlag VirtualFlag CoreType
    | PctfMethod (Loc Label) PrivateFlag VirtualFlag CoreType
    | PctfConstraint (CoreType, CoreType)
    | PctfAttribute Attribute
    | PctfExtension Extension

data ClassExpr = ClassExpr
    { pclDesc :: ClassExprDesc
    , pclLoc :: Location
    , pclAttributes :: Attributes
    }

data ClassExprDesc
    = PclConstr (Loc Longident) [CoreType]
    | PclStructure ClassStructure
    | PclFun ArgLabel (Maybe Expression) Pattern ClassExpr
    | PclApply ClassExpr [(ArgLabel, Expression)]
    | PclLet RecFlag [ValueBinding] ClassExpr
    | PclConstraint ClassExpr ClassType
    | PclExtension Extension
    | PclOpen OverrideFlag (Loc Longident) ClassExpr


-- Expression Types

data Expression = Expression
    { pexpDesc :: ExpressionDesc
    , pexpLoc :: Location
    , pexpAttributes :: Attributes
    }

data ExpressionDesc
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

-- Package Type

type PackageType = (Loc Longident, [(Loc Longident, CoreType)])

-- Object Field Types

data ObjectField
  = Otag (Location Label) Attributes CoreType
  | Oinherit CoreType

-- Loc Types

type Loc = Location

-- Row Field Types

data RowField
  = Rtag (Loc Label) Attributes Bool [CoreType]
  | Rinherit CoreType

-- Value Binding Types

data ValueBinding = ValueBinding
    { pvbPat :: Pattern
    , pvbExpr :: Expression
    , pvbAttributes :: Attributes
    , pvbLoc :: Location
    }

-- Case Types

data Case = Case
    { pcLhs :: Pattern
    , pcGuard :: Maybe Expression
    , pcRhs :: Expression
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

data ExtensionConstructor = ExtensionConstructor
  {
    pextName :: Loc String
  , pextKind :: ExtensionConstructorKind
  , pextLoc :: Location
  , pextAttributes :: Attributes
  }

data ExtensionConstructorKind =
    PextDecl ConstructorArguments (Maybe CoreType)
  | PextRebind (Loc Longident)


-- Constructor Declaration and Arguments Types

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

-- AST TYPES --

-- Constant Types

data Constant = 
    PconstInteger String (Maybe Char)
  | PconstChar Int
  | PconstString String (Maybe String)
  | PconstFloat String (Maybe Char)

-- RecFlag Types

data RecFlag = Nonrecursive | Recursive

-- Direction Flag Types

data DirectionFlag = Upto | Downto

-- Private Flag Types

data PrivateFlag
  = Private
  | Public

-- Mutable Flag Types

data MutableFlag = Immutable | Mutable

-- Virtual Flag Types

data VirtualFlag = Virtual | Concrete

-- Override Flag Types

data OverrideFlag = Override | Fresh

-- Closed Flag Types

data ClosedFlag
  = Closed
  | Open

-- Label

type Label = String

-- ArgLabel Types

data ArgLabel
  = Nolabel
  | Labelled String
  | Optional String

-- Loc Type

type Loc a = 
    { txt :: a
    , loc :: Location
    }

-- Variance Types

data Variance
  = Covariant
  | Contravariant
  | Invariant
