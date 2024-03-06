#![allow(dead_code)]
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourcePos {
    pub src_line: i32,
    pub src_column: i32,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceRange {
    pub src_start: SourcePos,
    pub src_end: SourcePos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comment<L: Debug + PartialEq> {
    Comment(String),
    Space(i32),
    Line(L),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LineFeed {
    LF,
    CRLF,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenAnn {
    pub tok_range: SourceRange,
    pub tok_leading_comments: Vec<Comment<LineFeed>>,
    pub tok_trailing_comments: Vec<Comment<()>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SourceStyle {
    ASCII,
    Unicode,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Token {
    TokLeftParen,
    TokRightParen,
    TokLeftBrace,
    TokRightBrace,
    TokLeftSquare,
    TokRightSquare,
    TokLeftArrow(SourceStyle),
    TokRightArrow(SourceStyle),
    TokRightFatArrow(SourceStyle),
    TokDoubleColon(SourceStyle),
    TokForall(SourceStyle),
    TokEquals,
    TokPipe,
    TokTick,
    TokDot,
    TokComma,
    TokUnderscore,
    TokBackslash,
    TokLowerName(Vec<String>, String),
    TokUpperName(Vec<String>, String),
    TokOperator(Vec<String>, String),
    TokSymbolName(Vec<String>, String),
    TokSymbolArr(SourceStyle),
    TokHole(String),
    TokChar(String, char),
    TokString(String, String),
    TokRawString(String),
    TokInt(String, i64),
    TokNumber(String, f64),
    TokLayoutStart,
    TokLayoutSep,
    TokLayoutEnd,
    TokEof,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct SourceToken {
    pub tok_ann: TokenAnn,
    pub tok_value: Token,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub get_ident: String,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Name<A> {
    pub name_tok: SourceToken,
    pub name_value: A,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct QualifiedName<A> {
    pub qual_tok: SourceToken,
    pub qual_module: Option<ModuleName>,
    pub qual_name: A,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Label {
    pub lbl_tok: SourceToken,
    pub lbl_name: String,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Wrapped<A> {
    pub wrp_open: SourceToken,
    pub wrp_value: A,
    pub wrp_close: SourceToken,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Separated<A> {
    pub sep_head: A,
    pub sep_tail: Vec<(SourceToken, A)>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Labeled<A, B> {
    pub lbl_label: A,
    pub lbl_sep: SourceToken,
    pub lbl_value: B,
}

pub type Delimited<A> = Wrapped<Option<Separated<A>>>;
pub type DelimitedNonEmpty<A> = Wrapped<Separated<A>>;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum OneOrDelimited<A> {
    One(A),
    Many(DelimitedNonEmpty<A>),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Type<A> {
    TypeVar(A, Name<Ident>),
    TypeConstructor(A, QualifiedName<String>), // String works ???
    TypeWildcard(A, SourceToken),
    TypeHole(A, Name<Ident>),
    TypeString(A, SourceToken, String),
    TypeInt(A, Option<SourceToken>, SourceToken, i64),
    TypeRow(A, Wrapped<Row<A>>),
    TypeRecord(A, Wrapped<Row<A>>),
    TypeForall(A, SourceToken, Vec<TypeVarBinding<A>>, SourceToken, Box<Type<A>>), // Box ???
    TypeKinded(A, Box<Type<A>>, SourceToken, Box<Type<A>>), // Box ???
    TypeApp(A, Box<Type<A>>, Box<Type<A>>), // Box ???
    TypeOp(A, Box<Type<A>>, QualifiedName<String>, Box<Type<A>>), // String works ???
    TypeOpName(A, QualifiedName<String>), // String works ???
    TypeArr(A, Box<Type<A>>, SourceToken, Box<Type<A>>), // Box ???
    TypeArrName(A, SourceToken),
    TypeConstrained(A, Constraint<A>, SourceToken, Box<Type<A>>),
    TypeParens(A, Wrapped<Box<Type<A>>>), // Box ???
    TypeUnaryRow(A, SourceToken, Box<Type<A>>), // Box ???
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum TypeVarBinding<A> {
    TypeVarKinded(Wrapped<Labeled<(Option<SourceToken>, Name<Ident>), Type<A>>>),
    TypeVarName((Option<SourceToken>, Name<Ident>)),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Constraint<A> {
    Constraint(A, QualifiedName<String>, Vec<Type<A>>), // String works ???
    ConstraintParens(A, Wrapped<Box<Constraint<A>>>), // Box ???
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Row<A> {
    pub row_labels: Option<Separated<Labeled<Label, Box<Type<A>>>>>,
    pub row_tail: Option<(SourceToken, Box<Type<A>>)>,
}


// Module Name to be done ???
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleName(pub String);


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Module<A> {
    pub mod_ann: A,
    pub mod_keyword: SourceToken,
    pub mod_namespace: Name<ModuleName>,
    pub mod_exports: Option<DelimitedNonEmpty<Export<A>>>,
    pub mod_where: SourceToken,
    pub mod_imports: Vec<ImportDecl<A>>,
    pub mod_decls: Vec<Declaration<A>>,
    pub mod_trailing_comments: Vec<Comment<LineFeed>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Export<A> {
    ExportValue(A, Name<Ident>),
    ExportOp(A, Name<String>), // String Works ???
    ExportType(A, Name<String>, Option<DataMembers<A>>), // String Works ???
    ExportTypeOp(A, SourceToken, Name<String>), // String Works ???
    ExportClass(A, SourceToken, Name<String>), // String Works ???
    ExportModule(A, SourceToken, Name<ModuleName>),
}
// Making New Types
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DataMembers<T> {
    DataAll(T, SourceToken),
    DataEnumerated(T, Delimited<Name<N.ProperName<'static, N.ConstructorName>>>), 
} 

// Assuming types like DataHead, SourceToken, Separated, DataCtor, Type, Name, N.ProperName, ClassHead, NonEmpty, Labeled, Instance, InstanceHead, ValueBindingFields, FixityFields, Foreign, and Role are defined
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Declaration<T> {
    DeclData(T, DataHead<T>, Option<(SourceToken, Separated<DataCtor<T>>)>),
    DeclType(T, DataHead<T>, SourceToken, Type<T>),
    DeclNewtype(T, DataHead<T>, SourceToken, Name<N.ProperName<'static, N.ConstructorName>>, Type<T>), 
    DeclClass(T, ClassHead<T>, Option<(SourceToken, NonEmpty<Labeled<Name<Ident>, Type<T>>>)>),
    DeclInstanceChain(T, Separated<Instance<T>>),
    DeclDerive(T, SourceToken, Option<SourceToken>, InstanceHead<T>),
    DeclKindSignature(T, SourceToken, Labeled<Name<N.ProperName<'static, N.TypeName>>, Type<T>>),
    DeclSignature(T, Labeled<Name<Ident>, Type<T>>),
    DeclValue(T, ValueBindingFields<T>),
    DeclFixity(T, FixityFields),
    DeclForeign(T, SourceToken, SourceToken, Foreign<T>),
    DeclRole(T, SourceToken, SourceToken, Name<N.ProperName<'static, N.TypeName>>, NonEmpty<Role>) 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Instance<T> {
    instHead: InstanceHead<T>,
    instBody: Option<(SourceToken, NonEmpty<InstanceBinding<T>>)>, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum InstanceBinding<T> {
    InstanceBindingSignature(T, Labeled<Name<Ident>, Type<T>>),
    InstanceBindingName(T, ValueBindingFields<T>),
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ImportDecl<T> {
    impAnn: T,
    impKeyword: SourceToken,
    impModule: Name<N.ModuleName>,
    impNames: Option<(Maybe<SourceToken>, DelimitedNonEmpty<Import<T>>)>,
    impQual: Option<(SourceToken, Name<N.ModuleName>)>, 
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Import<T> {
    ImportValue(T, Name<Ident>),
    ImportOp(T, Name<N.OpName<'static, N.ValueOpName>>), 
    ImportType(T, Name<N.ProperName<'static, N.TypeName>>, Option<DataMembers<T>>),
    ImportTypeOp(T, SourceToken, Name<N.OpName<'static, N.TypeOpName>>),
    ImportClass(T, SourceToken, Name<N.ProperName<'static, N.ClassName>>) 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DataHead<T> {
    dataHdKeyword: SourceToken,
    dataHdName: Name<N.ProperName<'static, N.TypeName>>,
    dataHdVars: Vec<TypeVarBinding<T>>, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DataCtor<T> {
    dataCtorAnn: T,
    dataCtorName: Name<N.ProperName<'static, N.ConstructorName>>,
    dataCtorFields: Vec<Type<T>>, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ClassHead<T> {
    clsKeyword: SourceToken,
    clsSuper: Option<(OneOrDelimited<Constraint<T>>, SourceToken)>,
    clsName: Name<N.ProperName<'static, N.ClassName>>,
    clsVars: Vec<TypeVarBinding<T>>,
    clsFundeps: Option<(SourceToken, Separated<ClassFundep>)>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum ClassFundep {
    FundepDetermined(SourceToken, NonEmpty<Name<Ident>>),
    FundepDetermines(NonEmpty<Name<Ident>>, SourceToken, NonEmpty<Name<Ident>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct InstanceHead<T> {
    instKeyword: SourceToken,
    instNameSep: Option<(Name<Ident>, SourceToken)>,
    instConstraints: Option<(OneOrDelimited<Constraint<T>>, SourceToken)>,
    instClass: QualifiedName<N.ProperName<'static, N.ClassName>>,
    instTypes: Vec<Type<T>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Fixity {
    Infix,
    Infixl,
    Infixr,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum FixityOp {
    FixityValue(QualifiedName<Either<Ident, N.ProperName<'static, N.ConstructorName>>>, SourceToken, Name<N.OpName<'static, N.ValueOpName>>), 
    FixityType(SourceToken, QualifiedName<N.ProperName<'static, N.TypeName>>, SourceToken, Name<N.OpName<'static, N.TypeOpName>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct FixityFields {
    fxtKeyword: (SourceToken, Fixity),
    fxtPrec: (SourceToken, Integer),
    fxtOp: FixityOp,
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ValueBindingFields<T> {
    valName: Name<Ident>,
    valBinders: Vec<Binder<T>>,
    valGuarded: Guarded<T>,
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Guarded<T> {
    Unconditional(SourceToken, Where<T>),
    Guarded(NonEmpty<GuardedExpr<T>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct GuardedExpr<T> {
    grdBar: SourceToken,
    grdPatterns: Separated<PatternGuard<T>>,
    grdSep: SourceToken,
    grdWhere: Where<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct PatternGuard<T> {
    patBinder: Option<(Binder<T>, SourceToken)>,
    patExpr: Expr<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Foreign<T> {
    ForeignValue(Labeled<Name<Ident>, Type<T>>),
    ForeignData(SourceToken, Labeled<Name<N.ProperName<'static, N.TypeName>>, Type<T>>), 
    ForeignKind(SourceToken, Name<N.ProperName<'static, N.TypeName>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Role {
    roleTok: SourceToken,
    roleValue: R::Role, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr<T> {
    ExprHole(T, Name<Ident>),
    ExprSection(T, SourceToken),
    ExprIdent(T, QualifiedName<Ident>),
    ExprConstructor(T, QualifiedName<N.ProperName<'static, N.ConstructorName>>), 
    ExprBoolean(T, SourceToken, Bool), 
    ExprChar(T, SourceToken, Char),
    ExprString(T, SourceToken, PSString), 
    ExprNumber(T, SourceToken, Either<Integer, Double>), 
    ExprArray(T, Delimited<Expr<T>>),
    ExprRecord(T, Delimited<RecordLabeled<Expr<T>>>),
    ExprParens(T, Wrapped<Expr<T>>),
    ExprTyped(T, Expr<T>, SourceToken, Type<T>),
    ExprInfix(T, Expr<T>, Wrapped<Expr<T>>, Expr<T>),
    ExprOp(T, Expr<T>, QualifiedName<N.OpName<'static, N.ValueOpName>>, Expr<T>), 
    ExprOpName(T, QualifiedName<N.OpName<'static, N.ValueOpName>>),
    ExprNegate(T, SourceToken, Expr<T>),
    ExprRecordAccessor(T, RecordAccessor<T>),
    ExprRecordUpdate(T, Expr<T>, DelimitedNonEmpty<RecordUpdate<T>>),
    ExprApp(T, Expr<T>, Expr<T>),
    ExprVisibleTypeApp(T, Expr<T>, SourceToken, Type<T>),
    ExprLambda(T, Lambda<T>),
    ExprIf(T, IfThenElse<T>),
    ExprCase(T, CaseOf<T>),
    ExprLet(T, LetIn<T>),
    ExprDo(T, DoBlock<T>),
    ExprAdo(T, AdoBlock<T>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RecordLabeled<T> {
    RecordPun(Name<Ident>), 
    RecordField(Label, SourceToken, T), 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RecordUpdate<T> {
    RecordUpdateLeaf(Label, SourceToken, Expr<T>),
    RecordUpdateBranch(Label, DelimitedNonEmpty<RecordUpdate<T>>), 
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct RecordAccessor<T> {
    recExpr: Expr<T>,
    recDot: SourceToken,
    recPath: Separated<Label>, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Lambda<T> {
    lmbSymbol: SourceToken,
    lmbBinders: NonEmpty<Binder<T>>,
    lmbArr: SourceToken,
    lmbBody: Expr<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct IfThenElse<T> {
    iteIf: SourceToken,
    iteCond: Expr<T>,
    iteThen: SourceToken,
    iteTrue: Expr<T>,
    iteElse: SourceToken,
    iteFalse: Expr<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct CaseOf<T> {
    caseKeyword: SourceToken,
    caseHead: Separated<Expr<T>>,
    caseOf: SourceToken,
    caseBranches: NonEmpty<(Separated<Binder<T>>, Guarded<T>)>, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct LetIn<T> {
    letKeyword: SourceToken,
    letBindings: NonEmpty<LetBinding<T>>,
    letIn: SourceToken,
    letBody: Expr<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Where<T> {
    whereExpr: Expr<T>,
    whereBindings: Option<(SourceToken, NonEmpty<LetBinding<T>>)>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum LetBinding<T> {
    LetBindingSignature(T, Labeled<Name<Ident>, Type<T>>),
    LetBindingName(T, ValueBindingFields<T>),
    LetBindingPattern(T, Binder<T>, SourceToken, Where<T>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DoBlock<T> {
    doKeyword: SourceToken,
    doStatements: NonEmpty<DoStatement<T>>, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum DoStatement<T> {
    DoLet(SourceToken, NonEmpty<LetBinding<T>>),
    DoDiscard(Expr<T>),
    DoBind(Binder<T>, SourceToken, Expr<T>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct AdoBlock<T> {
    adoKeyword: SourceToken,
    adoStatements: Vec<DoStatement<T>>, // Using Vec for lists
    adoIn: SourceToken,
    adoResult: Expr<T>, 
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Binder<T> {
    BinderWildcard(T, SourceToken),
    BinderVar(T, Name<Ident>),
    BinderNamed(T, Name<Ident>, SourceToken, Binder<T>),
    BinderConstructor(T, QualifiedName<N.ProperName<'static, N.ConstructorName>>, Vec<Binder<T>>),
    BinderBoolean(T, SourceToken, Bool), 
    BinderChar(T, SourceToken, Char),
    BinderString(T, SourceToken, PSString),
    BinderNumber(T, Option<SourceToken>, SourceToken, Either<Integer, Double>), 
    BinderArray(T, Delimited<Binder<T>>),
    BinderRecord(T, Delimited<RecordLabeled<Binder<T>>>),
    BinderParens(T, Wrapped<Binder<T>>),
    BinderTyped(T, Binder<T>, SourceToken, Type<T>),
    BinderOp(T, Binder<T>, QualifiedName<N.OpName<'static, N.ValueOpName>>, Binder<T>), 
}