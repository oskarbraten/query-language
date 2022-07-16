use std::ops::{Bound, Deref};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    And,
    Or,
    Not,
    Boost,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    ParenOpen,
    ParenClose,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub value: String,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(value: &str, offset: usize) -> Self {
        Self {
            value: value.to_string(),

            start: (value.as_ptr() as usize) - offset,
            end: ((value.as_ptr() as usize) + value.len()) - offset,
        }
    }
}

impl Deref for Span {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Phrase(Span),
    Regex(Span),
    Range(Bound<Span>, Bound<Span>),
    Term(Span, Option<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(Span),
    Literal(Literal),
    Operator(Operator),
    Punctuation(Punctuation),
    Eof,
}
