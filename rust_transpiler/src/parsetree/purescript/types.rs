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


