use regex::Regex;
use std::ops::Range;
use time::OffsetDateTime;

#[derive(Debug, Clone, Copy)]
pub enum Occurance {
    Required,
    Forbidden,
}

#[derive(Debug, Clone)]
pub enum Primitive<'a> {
    String(&'a str),
    Date(OffsetDateTime),
    F64(f64),
    I64(i64),
    U64(u64),
}

#[derive(Debug, Clone)]
pub enum ClauseBody<'a> {
    Term(Primitive<'a>),
    Phrase(Vec<&'a str>),
    Range(Range<Primitive<'a>>),
    Regex(Regex),
    Fuzzy(&'a str, u8),

    Expression(Expression<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum FieldKind {
    String { tokenized: bool },
    Date,
    F64,
    I64,
    U64,
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    name: &'a str,
    kind: FieldKind,
}

#[derive(Debug, Clone)]
pub struct Clause<'a> {
    occurance: Option<Occurance>,
    field: Option<Field<'a>>,
    variant: ClauseBody<'a>,
}

#[derive(Debug, Clone)]
pub struct Expression<'a>(Vec<Clause<'a>>);
