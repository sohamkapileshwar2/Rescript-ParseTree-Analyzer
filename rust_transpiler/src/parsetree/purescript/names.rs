#![allow(dead_code)]
#![allow(non_snake_case)]

use core::str;
use std::{fmt::Debug, marker::PhantomData};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProperNameData {
    pub runProperName: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProperName<A> {
    ProperName(ProperNameData, PhantomData<A>),
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProperNameType {
    TypeName,
    ConstructorName,
    ClassName,
    Namespace,
}

// OpName

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OpNameData {
    pub runOpName: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpName<A> {
    OpName(OpNameData, PhantomData<A>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpNameType {
    ValueOpName,
    TypeOpName,
    AnyOpName,
}

// ModuleName

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ModuleName {
    ModuleName(String),
}

// PSString

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PSStringData {
    pub toUTF16CodeUnits: Vec<u16>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PSString {
    PSString(PSStringData),
}