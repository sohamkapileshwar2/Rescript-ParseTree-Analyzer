#![allow(non_snake_case)]
// use serde::{Serialize, Deserialize};


type Structure = Vec<StructureItem>;

#[derive( Debug, PartialEq)]
struct StructureItem {
    pstr_desc: StructureItemDesc, // Assuming StructureItemDesc is another type you'll define
    pstr_loc: Location,            // Assuming Location is another type you'll define
}

#[derive( Debug, PartialEq)]
enum StructureItemDesc {
    PstrEval(Expression, Attributes),
    PstrValue(RecFlag, Vec<ValueBinding>),
    PstrPrimitive(ValueDescription),
    PstrType(RecFlag, Vec<TypeDeclaration>),
    PstrTypext(TypeExtension),
    PstrException(ExtensionConstructor),
    PstrModule(ModuleBinding),
    PstrRecmodule(Vec<ModuleBinding>),
    PstrModtype(ModuleTypeDeclaration),
    PstrOpen(OpenDescription),
    PstrClass(()), // Assuming this is a placeholder for something more specific
    PstrClassType(Vec<ClassTypeDeclaration>),
    PstrInclude(IncludeDeclaration),
    PstrAttribute(Attribute),
    PstrExtension(Extension, Attributes),
}


// Location Types

#[derive( Debug, PartialEq)]
struct Location {
    locStart: Position, // Assuming Position is another type you'll define
    locEnd: Position,
    locGhost: bool,
}

#[derive( Debug, PartialEq)]
struct Position {
    posFname: String,
    posLnum: i32,
    posBol: i32,
    posCnum: i32,
}

// Longident Types

#[derive( Debug, PartialEq)]
enum Longident {
    Lident(String),
    Ldot(Box<Longident>, String),
    Lapply(Box<Longident>, Box<Longident>),
}

// Module Types

#[derive( Debug, PartialEq)]
struct ModuleBinding {
    pmbName: Loc<String>,
    pmbExpr: ModuleExpr,
    pmbAttributes: Attributes,
    pmbLoc: Location,
}

#[derive( Debug, PartialEq)]
struct ModuleExpr {
    pmodDesc: ModuleExprDesc, // Direct use, assuming you'll define or replace
    pmodLoc: Location, // Previously defined
    pmodAttributes: Attributes, // Direct use, keeping track of previous context
}

#[derive( Debug, PartialEq)]
enum ModuleExprDesc {
    PmodIdent(Loc<Longident>),
    PmodStructure(Structure),
    PmodFunctor(Loc<String>, Option<ModuleType>, Box<ModuleExpr>),
    PmodApply(Box<ModuleExpr>, Box<ModuleExpr>),
    PmodConstraint(Box<ModuleExpr>, ModuleType),
    PmodUnpack(Expression),
    PmodExtension(Extension),
}

#[derive( Debug, PartialEq)]
struct ModuleType {
    pmtyDesc: Box<ModuleTypeDesc>, // Direct naming, assuming definition elsewhere
    pmtyLoc: Location, // Using the previously defined Location type
    pmtyAttributes: Attributes, // Direct naming, assuming definition elsewhere
}

#[derive( Debug, PartialEq)]
enum ModuleTypeDesc {
    PmtyIdent(Loc<Longident>),
    PmtySignature(Signature),
    PmtyFunctor(Loc<String>, Option<ModuleType>, ModuleType),
    PmtyWith(ModuleType, WithConstraint),
    PmtyTypeof(Expression),
    PmtyExtension(Extension),
}

#[derive( Debug, PartialEq)]
struct ModuleDeclaration {
    pmdName: Loc<String>,
    pmdType: Option<ModuleType>,
    pmdAttributes: Attributes,
    pmdLoc: Location,
}

#[derive( Debug, PartialEq)]
struct ModuleTypeDeclaration {
    pmtdName: Loc<String>,
    pmtdType: Option<ModuleType>,
    pmtdAttributes: Attributes,
    pmtdLoc: Location,
}

// Open Desctiption

#[derive( Debug, PartialEq)]
struct OpenDescription {
    popenLid: Loc<Longident>,
    popenOverride: bool,
    popenLoc: Location,
    popenAttributes: Attributes,
}

// Include Declaration
type IncludeDescription = IncludeInfos<ModuleType>;

#[derive( Debug, PartialEq)]
struct IncludeInfos<T> {
    pinclMod: T,
    pinclLoc: Location,
    pinclAttributes: Attributes,
}

type IncludeDeclaration =  IncludeInfos<ModuleExpr>;

#[derive( Debug, PartialEq)]
enum WithConstraint {
    PwithType(Loc<Longident>, TypeDeclaration),
    PwithModule(Loc<Longident>, Loc<Longident>),
    PwithTypeSubst(Loc<Longident>, TypeDeclaration),
    PwithModSubst(Loc<Longident>, Loc<Longident>),
}

// Class Types

type ClassTypeDeclaration = ClassInfos<ClassType>;

#[derive( Debug, PartialEq)]
struct ClassInfos<T> {
    pciVirt: VirtualFlag,
    pciParams: Vec<(CoreType, Variance)>,
    pciName: Loc<String>,
    pciExpr: T,
    pciLoc: Location,
    pciAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
struct ClassStructure {
    pcstrSelf: Pattern,
    pcstrFields: Vec<ClassField>,
}

#[derive( Debug, PartialEq)]
struct ClassField {
    pcfDesc: ClassFieldDesc,
    pcfLoc: Location,
    pcfAttributes: Attributes,
}


#[derive( Debug, PartialEq)]
enum ClassFieldDesc {
    PcfInherit(()),
    PcfVal(Loc<Label>, MutableFlag, ClassFieldKind),
    PcfMethod(Loc<Label>, PrivateFlag, ClassFieldKind),
    PcfConstraint(CoreType, CoreType),
    PcfInitializer(Expression),
    PcfAttribute(Attribute),
    PcfExtension(Extension),
}

#[derive( Debug, PartialEq)]
enum ClassFieldKind {
    CfkVirtual(CoreType),
    CfkConcrete(OverrideFlag, Expression),
}

#[derive( Debug, PartialEq)]
struct ClassType {
    pctyDesc: Box<ClassTypeDesc>,
    pctyLoc: Location,
    pctyAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum ClassTypeDesc {
    PctyConstr(Loc<Longident>, Vec<CoreType>),
    PctySignature(ClassSignature),
    PctyArrow(ArgLabel, CoreType, ClassType),
    PctyExtension(Extension),
    PctyOpen(OverrideFlag, Loc<Longident>, ClassType),
}

#[derive( Debug, PartialEq)]
struct ClassSignature {
    pcsigSelf: CoreType,
    pcsigFields: Vec<ClassTypeField>,
}

#[derive( Debug, PartialEq)]
struct ClassTypeField {
    pctfDesc: ClassTypeFieldDesc,
    pctfLoc: Location,
    pctfAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum ClassTypeFieldDesc {
    PctfInherit(ClassType),
    PctfVal(Loc<Label>, MutableFlag, CoreType),
    PctfMethod(Loc<Label>, PrivateFlag, CoreType),
    PctfConstraint(CoreType, CoreType),
    PctfAttribute(Attribute),
    PctfExtension(Extension),
}

#[derive( Debug, PartialEq)]
struct ClassExpr {
    pclDesc: Box<ClassExprDesc>,
    pclLoc: Location,
    pclAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum ClassExprDesc {
    PclStructure(ClassStructure),
    PclFunctor(Loc<String>, Option<ModuleType>, ClassExpr),
    PclApply(ClassExpr, ClassExpr),
    PclLet(RecFlag, Vec<ValueBinding>, ClassExpr),
    PclConstraint(ClassExpr, ClassType),
    PclExtension(Extension),
}

// Expression Types

#[derive( Debug, PartialEq)]
struct Expression {
    pexpDesc: Box<ExpressionDesc>,
    pexpLoc: Location,
    pexpAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum ExpressionDesc {
    PexpIdent(Loc<Longident>),
    PexpConstant(Constant),
    PexpLet(RecFlag, Vec<ValueBinding>, Box<Expression>),
    PexpFunction(Vec<Case>),
    PexpFun(ArgLabel, Option<Expression>, Pattern, Box<Expression>),
    PexpApply(Expression, Vec<Expression>),
    PexpMatch(Expression, Vec<Case>, Option<Expression>),
    PexpTry(Expression, Vec<Case>),
    PexpTuple(Vec<Expression>),
    PexpConstruct(Loc<Longident>, Option<Expression>),
    PexpVariant(Label, Option<Expression>),
    PexpRecord(Vec<(Loc<Longident>, Expression)>, Option<Expression>),
    PexpField(Expression, Loc<Longident>),
    PexpSetfield(Expression, Loc<Longident>, Expression),
    PexpArray(Vec<Expression>),
    PexpIfthenelse(Expression, Expression, Option<Expression>),
    PexpSequence(Expression, Expression),
    PexpWhile(Expression, Expression),
    PexpFor(Pattern, Expression, Expression, DirectionFlag, Expression),
    PexpConstraint(Expression, CoreType),
    PexpCoerce(Expression, Option<CoreType>, CoreType),
    PexpSend(Expression, Loc<Label>),
    PexpNew(Loc<Longident>),
    PexpSetinstvar(Loc<Label>, Expression),
    PexpOverride(Vec<(Loc<Label>, Expression)>),
    PexpLetmodule(Loc<String>, ModuleExpr, Expression),
    PexpLetexception(ExtensionConstructor, Expression),
    PexpAssert(Expression),
    PexpLazy(Expression),
    PexpPoly(Expression, Option<CoreType>),
    PexpObject(ClassStructure),
    PexpNewtype(Loc<String>, Expression),
    PexpPack(ModuleExpr),
    PexpOpen(OverrideFlag, Loc<Longident>, Expression),
    PexpExtension(Extension),
    PexpUnreachable,
}

// Attribute Types

type Attributes = Vec<Attribute>;

type Attribute = (Loc<String>, Payload);

// Payload Types

#[derive( Debug, PartialEq)]
enum Payload {
    PStr(Structure),
    PSig(Signature),
    PTyp(CoreType),
    PPat(Pattern, Option<Expression>),
}

// Signature Types
type Signature = Vec<SignatureItem>;

#[derive( Debug, PartialEq)]
struct SignatureItem {
    psigDesc: SignatureItemDesc,
    psigLoc: Location,
}

#[derive( Debug, PartialEq)]
enum SignatureItemDesc {
    PsigValue(ValueDescription),
    PsigType(TypeDeclaration),
    PsigTypext(TypeExtension),
    PsigException(ExtensionConstructor),
    PsigModule(ModuleDeclaration),
    PsigRecmodule(Vec<ModuleDeclaration>),
    PsigModtype(ModuleTypeDeclaration),
    PsigOpen(OpenDescription),
    PsigInclude(IncludeDeclaration),
    PsigClass(()),
    PsigClassType(ClassTypeDeclaration),
    PsigAttribute(Attribute),
    PsigExtension(Extension),
}

// Core Types

#[derive( Debug, PartialEq)]
struct CoreType {
    ptypDesc: Box<CoreTypeDesc>,
    ptypLoc: Location,
    ptypAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum CoreTypeDesc {
    PtypAny,
    PtypVar(String),
    PtypArrow(ArgLabel, CoreType, CoreType),
    PtypTuple(Vec<CoreType>),
    PtypConstr(Loc<Longident>, Vec<CoreType>),
    PtypObject(Vec<ObjectField>, ClosedFlag),
    PtypClass(Loc<Longident>, Vec<CoreType>),
    PtypAlias(CoreType, String),
    PtypVariant(Vec<RowField>, ClosedFlag, Option<Vec<Label>>),
    PtypPoly(Vec<Loc<String>>, CoreType),
    PtypPackage(PackageType),
    PtypExtension(Extension),
}

// Pakage type
type PackageType = (Loc<Longident>, Vec<(Loc<Longident>, CoreType)>);


// Object Fields Types

#[derive( Debug, PartialEq)]
enum ObjectField {
    Otag(Loc<Label>, Attributes, CoreType),
    Oinherit(CoreType),
}

#[derive( Debug, PartialEq)]
enum RowField {
    Rtag(Loc<Label>, Attributes, bool, Vec<CoreType>),
    Rinherit(CoreType),
}

#[derive( Debug, PartialEq)]
struct ValueBinding {
    pvbPat: Pattern,
    pvbExpr: Expression,
    pvbAttributes: Attributes,
    pvbLoc: Location,
}

#[derive( Debug, PartialEq)]
struct Case {
    pcLhs: Pattern,
    pcGuard: Option<Expression>,
    pcRhs: Expression,
}
#[derive( Debug, PartialEq)]
struct Pattern {
    ppatDesc: Box<PatternDesc>,
    ppatLoc: Location,
    ppatAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum PatternDesc {
    PpatAny,
    PpatVar(Loc<String>),
    PpatAlias(Pattern, Loc<String>),
    PpatConstant(Constant),
    PpatInterval(Constant, Constant),
    PpatTuple(Vec<Pattern>),
    PpatConstruct(Loc<Longident>, Option<Pattern>),
    PpatVariant(Label, Option<Pattern>),
    PpatRecord(Vec<(Loc<Longident>, Pattern)>, ClosedFlag),
    PpatArray(Vec<Pattern>),
    PpatOr(Pattern, Pattern),
    PpatConstraint(Pattern, CoreType),
    PpatType(Loc<Longident>),
    PpatLazy(Pattern),
    PpatUnpack(Loc<String>),
    PpatException(Pattern),
    PpatExtension(Extension),
    PpatOpen(Loc<Longident>, Pattern),
}

#[derive( Debug, PartialEq)]
struct Extension(Loc<String>, Payload);

#[derive( Debug, PartialEq)]
struct ExtensionConstructor {
    pextName: Loc<String>,
    pextKind: ExtensionConstructorKind,
    pextLoc: Location,
    pextAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum ExtensionConstructorKind {
    PextDecl(ConstructorArguments, Option<CoreType>),
    PextRebind(Loc<Longident>),
}


#[derive( Debug, PartialEq)]
struct ConstructorDeclaration {
    pcdName: Loc<String>,
    pcdArgs: ConstructorArguments,
    pcdRes: Option<CoreType>,
    pcdLoc: Location,
    pcdAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum ConstructorArguments {
    PcstrTuple(Vec<CoreType>),
    PcstrRecord(Vec<(Loc<Longident>, CoreType, MutableFlag)>),
}

#[derive( Debug, PartialEq)]
struct ValueDescription {
    pvalName: Loc<String>,
    pvalType: CoreType,
    pvalPrim: Vec<String>,
    pvalAttributes: Attributes,
    pvalLoc: Location,
}

#[derive( Debug, PartialEq)]
struct TypeDeclaration {
    ptypeName: Loc<String>,
    ptypeParams: Vec<Loc<String>>,
    ptypeCtxt: Vec<CoreType>,
    ptypeKind: TypeKind,
    ptypePrivate: PrivateFlag,
    ptypeManifest: Option<CoreType>,
    ptypeAttributes: Attributes,
    ptypeLoc: Location,
}

#[derive( Debug, PartialEq)]
struct TypeExtension {
    ptyextPath: Loc<Longident>,
    ptyextParams: Vec<(CoreType, Variance)>,
    ptyextConstructors: Vec<ExtensionConstructor>,
    ptyextPrivate: PrivateFlag,
    ptyextAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum TypeKind {
    PtypeAbstract,
    PtypeVariant(Vec<ConstructorDeclaration>),
    PtypeRecord(Vec<LabelDeclaration>),
    PtypeOpen,
}

#[derive( Debug, PartialEq)]
struct LabelDeclaration {
    pldName: Loc<String>,
    pldMutable: MutableFlag,
    pldType: CoreType,
    pldLoc: Location,
    pldAttributes: Attributes,
}

#[derive( Debug, PartialEq)]
enum Constant {
    PconstInteger(String, Option<char>),
    PconstChar(i32),
    PconstString(String, Option<String>),
    PconstFloat(String, Option<char>),
}

#[derive( Debug, PartialEq)]
enum RecFlag {
    Nonrecursive,
    Recursive,
}

#[derive( Debug, PartialEq)]
enum DirectionFlag {
    Upto,
    Downto,
}

#[derive( Debug, PartialEq)]
enum PrivateFlag {
    Private,
    Public,
}

#[derive( Debug, PartialEq)]
enum MutableFlag {
    Immutable,
    Mutable,
}

#[derive( Debug, PartialEq)]
enum VirtualFlag {
    Virtual,
    Concrete,
}

#[derive( Debug, PartialEq)]
enum OverrideFlag {
    Override,
    Fresh,
}

#[derive( Debug, PartialEq)]
enum ClosedFlag {
    Closed,
    Open,
}

type Label = String;

#[derive( Debug, PartialEq)]
enum ArgLabel {
    Nolabel,
    Labelled(String),
    Optional(String),
}


#[derive( Debug, PartialEq)]
struct Loc<T> {
    txt: T,
    loc: Location,
}

#[derive( Debug, PartialEq)]
enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}