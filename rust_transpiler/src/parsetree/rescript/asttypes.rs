use serde::{Serialize, Deserialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
enum Constant {
    ConstInt(i32),
    ConstChar(char),
    ConstString(String, Option<String>),
    ConstFloat(String),
    ConstInt32(i32),
    ConstInt64(i64),
    ConstNativeInt(isize),
}

#[derive(Serialize, Deserialize)]
enum RecFlag {
    Nonrecursive,
    Recursive,
}

#[derive(Serialize, Deserialize)]
enum DirectionFlag {
    Upto,
    Downto,
}

#[derive(Serialize, Deserialize)]
enum PrivateFlag {
    Private,
    Public,
}

#[derive(Serialize, Deserialize)]
enum MutableFlag {
    Immutable,
    Mutable,
}

#[derive(Serialize, Deserialize)]
enum VirtualFlag {
    Virtual,
    Concrete,
}

#[derive(Serialize, Deserialize)]
enum OverrideFlag {
    Override,
    Fresh,
}

#[derive(Serialize, Deserialize)]
enum ClosedFlag {
    Closed,
    Open,
}

// In Rust, a type alias can be used for simple renaming.
type Label = String;

#[derive(Serialize, Deserialize)]
enum ArgLabel {
    Nolabel,
    Labelled(String), // label:T -> ...
    Optional(String), // ?label:T -> ...
}

#[derive(Serialize, Deserialize)]
struct Loc<T> {
    txt: T,
    // Rust does not have a direct equivalent to OCaml's Location.t type,
    // so you would need to define it or use a placeholder type.
    // Here, we'll assume a simple struct for demonstration.
    loc: Location,
}

#[derive(Serialize, Deserialize)]
struct Location {
    // Placeholder fields for the location. In practice, you'll define these according to your needs.
    file: String,
    line: i32,
    column: i32,
}

#[derive(Serialize, Deserialize)]
enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}
