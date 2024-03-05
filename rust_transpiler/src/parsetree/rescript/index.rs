use serde::{Serialize, Deserialize};


type Structure = Vec<StructureItem>;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct StructureItem {
    pstr_desc: StructureItemDesc, // Assuming StructureItemDesc is another type you'll define
    pstr_loc: Location,            // Assuming Location is another type you'll define
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(tag = "type")]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Location {
    locStart: Position, // Assuming Position is another type you'll define
    locEnd: Position,
    locGhost: bool,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Position {
    posFname: String,
    posLnum: i32,
    posBol: i32,
    posCnum: i32,
}

// Longident Types

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum Longident {
    Lident(String),
    Ldot(Box<Longident>, String),
    Lapply(Box<Longident>, Box<Longident>),
}

// Module Types
 
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ModuleBinding {
    pmbName: Loc<String>, 
    pmbExpr: ModuleExpr, 
    pmbAttributes: Attributes,
    pmbLoc: Location, 
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ModuleExpr {
    pmodDesc: ModuleExprDesc, // Direct use, assuming you'll define or replace
    pmodLoc: Location, // Previously defined
    pmodAttributes: Attributes, // Direct use, keeping track of previous context
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ModuleExprDesc {
    PmodIdent(Loc<Longident>),
    PmodStructure(Structure), 
    PmodFunctor(Loc<String>, Option<ModuleType>, ModuleExpr),
    PmodApply(ModuleExpr, ModuleExpr),
    PmodConstraint(ModuleExpr, ModuleType),
    PmodUnpack(Expression),
    PmodExtension(Extension),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ModuleType {
    pmtyDesc: ModuleTypeDesc, // Direct naming, assuming definition elsewhere
    pmtyLoc: Location, // Using the previously defined Location type
    pmtyAttributes: Attributes, // Direct naming, assuming definition elsewhere
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ModuleTypeDesc {
    PmtyIdent(Loc<Longident>),
    PmtySignature(Signature),
    PmtyFunctor(Loc<String>, Option<ModuleType>, ModuleType),
    PmtyWith(ModuleType, WithConstraint),
    Pmty_typeof(Expression),
    Pmty_extension(Extension),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ModuleDecration {
    pmdName: Loc<String>,
    pmdType: Option<ModuleType>,
    pmdAttributes: Attributes,
    pmdLoc: Location,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ModuleTypeDeclaration {
    pmtdName: Loc<String>,
    pmtdType: Option<ModuleType>,
    pmtdAttributes: Attributes,
    pmtdLoc: Location,
}

// Open Desctiption

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct OpenDescription {
    popenLid: Loc<Longident>,
    popenOverride: bool,
    popenLoc: Location,
    popenAttributes: Attributes,
}

// Include Declaration
type IncludeDescription = IncludeInfos<ModuleType>;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct IncludeInfos<T> {
    pinclMod: T,
    pinclLoc: Location,
    pinclAttributes: Attributes,
}

type IncludeDeclaration =  IncludeInfos<ModuleExpr>;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum WithConstraint {
    PwithType(Loc<Longident>, TypeDeclaration),
    PwithModule(Loc<Longident>, Loc<Longident>),
    PwithTypeSubst(Loc<Longident>, TypeDeclaration),
    PwithModSubst(Loc<Longident>, Loc<Longident>),
}

// Class Types

type ClassTypeDeclaration = ClassInfos<ClassType>;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ClassInfos<T> {
    pciVirt: VirtualFlag,
    pciParams: Vec<(CoreType, Variance)>,
    pciName: Loc<String>,
    pciExpr: T,
    pciLoc: Location,
    pciAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ClassStructure {
    pcstrSelf: Pattern,
    pcstrFields: Vec<ClassField>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ClassField {
    pcfDesc: ClassFieldDesc,
    pcfLoc: Location,
    pcfAttributes: Attributes,
}


#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ClassFieldDesc {
    PcfInherit(()),
    PcfVal(Loc<Label>, MutableFlag, ClassFieldKind),
    PcfMethod(Loc<Label>, PrivateFlag, ClassFieldKind),
    PcfConstraint(CoreType, CoreType),
    PcfInitializer(Expression),
    PcfAttribute(Attribute),
    PcfExtension(Extension),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ClassFieldKind {
    CfkVirtual(CoreType),
    CfkConcrete(OverrideFlag, Expression),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ClassType {
    pctyDesc: ClassTypeDesc,
    pctyLoc: Location,
    pctyAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ClassTypeDesc {
    PctyConstr(Loc<Longident>, Vec<CoreType>),
    PctySignature(ClassSignature),
    PctyArrow(ArgLabel, CoreType, ClassType),
    PctyExtension(Extension),
    PctyOpen(OverrideFlag, Loc<Longident>, ClassType),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ClassSignature {
    pcsigSelf: CoreType,
    pcsigFields: Vec<ClassTypeField>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ClassTypeField {
    pctfDesc: ClassTypeFieldDesc,
    pctfLoc: Location,
    pctfAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ClassTypeFieldDesc {
    PctfInherit(ClassType),
    PctfVal(Loc<Label>, MutableFlag, CoreType),
    PctfMethod(Loc<Label>, PrivateFlag, CoreType),
    PctfConstraint(CoreType, CoreType),
    PctfAttribute(Attribute),
    PctfExtension(Extension),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ClassExpr {
    pclDesc: ClassExprDesc,
    pclLoc: Location,
    pclAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ClassExprDesc {
    PclStructure(ClassStructure),
    PclFunctor(Loc<String>, Option<ModuleType>, ClassExpr),
    PclApply(ClassExpr, ClassExpr),
    PclLet(RecFlag, Vec<ClassLetBinding>, ClassExpr),
    PclConstraint(ClassExpr, ClassType),
    PclExtension(Extension),
}

// Expression Types

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Expression {
    pexpDesc: ExpressionDesc,
    pexpLoc: Location,
    pexpAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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
    PexpRecord(Vec<(Loc<Longident>, Expression)>, Option<Expression>),
    PexpField(Expression, Loc<Longident>),
    PexpSetField(Expression, Loc<Longident>, Expression),
    PexpArray(Vec<Expression>),
    PexpIfThenElse(Expression, Expression, Option<Expression>),
    PexpSequence(Expression, Expression),
    PexpWhile(Expression, Expression),
    PexpFor(Loc<String>, Expression, Expression, DirectionFlag, Expression),
    PexpConstraint(Expression, CoreType),
    PexpCoerce(Expression, Option<CoreType>, CoreType),
    PexpSend(Expression, Loc<Longident>),
    PexpNew(Loc<Longident>),
    PexpSetField(Loc<Longident>, Loc<Longident>, Expression),
    PexpOverride(Vec<(Loc<Longident>, Expression)>),
    PexpLetModule(Loc<String>, ModuleExpr, Expression),
    PexpLetOpen(Loc<String>, Loc<Longident>, Expression),
    PexpExtension(Extension),
    PexpUnreachable(),
}

// Attribute Types

type Attributes = Vec<Attribute>;

type Attribute = (Loc<String>, Payload);

// Payload Types

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum Payload {
    PStr(Structure),
    PSig(Signature),
    PTyp(CoreType),
    PPat(Pattern, Option<Expression>),
}

// Signature Types
type Signature = Vec<SignatureItem>;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct SignatureItem {
    psigDesc: SignatureItemDesc,
    psigLoc: Location,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct CoreType {
    ptypDesc: CoreTypeDesc,
    ptypLoc: Location,
    ptypAttributes: Attributes,
}


#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ObjectField {
    Otag(Loc<Label>, Attributes, CoreType),
    Oinherit(CoreType),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum RowField {
    Rtag(Loc<Label>, Attributes, bool, Vec<CoreType>),
    Rinherit(CoreType),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ValueBinding {
    pvbPat: Pattern,
    pvbExpr: Expression,
    pvbAttributes: Attributes,
    pvbLoc: Location,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Case {
    pcLhs: Pattern,
    pcGuard: Option<Expression>,
    pcRhs: Expression,
}
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Pattern {
    ppatDesc: PatternDesc,
    ppatLoc: Location,
    ppatAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Extension(Loc<String>, Payload);

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ExtensionConstructor {
    pextName: Loc<String>,
    pextKind: ExtensionConstructorKind,
    pextLoc: Location,
    pextAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ExtensionConstructorKind {
    PextDecl(Vec<ExtensionConstructorArg>, Option<CoreType>),
    PextRebind(Loc<Longident>),
}


#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ConstructorDeclaration {
    pcdName: Loc<String>,
    pcdArgs: ConstructorArguments,
    pcdRes: Option<CoreType>,
    pcdLoc: Location,
    pcdAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ConstructorArguments {
    PcstrTuple(Vec<CoreType>),
    PcstrRecord(Vec<(Loc<Longident>, CoreType, MutableFlag)>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ValueDescription {
    pvalName: Loc<String>,
    pvalType: CoreType,
    pvalPrim: Vec<String>,
    pvalAttributes: Attributes,
    pvalLoc: Location,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct TypeDeclaration {
    ptypeName: Loc<String>,
    ptypeParams: Vec<Loc<String>>,
    ptypeCtxt: Vec<CoreType>,
    ptypeKind: TypeDeclarationKind,
    ptypePrivate: PrivateFlag,
    ptypeManifest: Option<CoreType>,
    ptypeAttributes: Attributes,
    ptypeLoc: Location,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct TypeExtension {
    ptyextPath: Loc<Longident>,
    ptyextParams: Vec<(CoreType, Variance)>,
    ptyextConstructors: Vec<ExtensionConstructor>,
    ptyextPrivate: PrivateFlag,
    ptyextAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum TypeKind {
    PtypeAbstract,
    PtypeVariant(Vec<ConstructorDeclaration>),
    PtypeRecord(Vec<LabelDeclaration>),
    PtypeOpen,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct LabelDeclaration {
    pldName: Loc<String>,
    pldMutable: MutableFlag,
    pldType: CoreType,
    pldLoc: Location,
    pldAttributes: Attributes,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum Constant {
    PconstInteger(String, Option<char>),
    PconstChar(i32),
    PconstString(String, Option<String>),
    PconstFloat(String, Option<char>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum RecFlag {
    Nonrecursive,
    Recursive,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum DirectionFlag {
    Upto,
    Downto,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum PrivateFlag {
    Private,
    Public,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum MutableFlag {
    Immutable,
    Mutable,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum VirtualFlag {
    Virtual,
    Concrete,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum OverrideFlag {
    Override,
    Fresh,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ClosedFlag {
    Closed,
    Open,
}

type Label = String;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum ArgLabel {
    Nolabel,
    Labelled(String),
    Optional(String),
}


#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Loc<T> {
    txt: T,
    loc: Location,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}