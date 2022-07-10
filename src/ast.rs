use std::ops::Range;
use time::OffsetDateTime;

use crate::lexer::*;

#[derive(Debug, Clone, Copy)]
pub enum Occurrence {
    Required,
    Forbidden,
}

#[derive(Debug, Clone)]
pub enum Primitive {
    String(String),
    Date(OffsetDateTime),
    F64(f64),
    I64(i64),
    U64(u64),
}

#[derive(Debug, Clone)]
pub enum ClauseBody {
    Term(Primitive, Option<u8>),
    Phrase(Vec<String>),
    Range(Range<Primitive>),
    Regex(String),

    Expression(Expression),
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
pub struct Field {
    pub name: String,
    pub kind: FieldKind,
}

#[derive(Debug, Clone)]
pub struct Clause {
    pub occurrence: Option<Occurrence>,
    pub field: Option<Field>,
    pub body: ClauseBody,
}

#[derive(Debug, Clone)]
pub struct Expression(Vec<Clause>);

pub struct ParserError<'a> {
    pub expected: Vec<Vec<Token<'static>>>,
    pub found: Option<Token<'a>>,
}

fn parse_clause<'a, 'b>(tokens: &mut Vec<Token<'a>>) -> Result<Clause, ParserError<'a>> {
    let (num, occurrence, identifier) = match &tokens[..] {
        [.., Token::Identifier(identifier), Token::Operator(Operator::Plus)] => {
            (2, Some(Occurrence::Required), Some(identifier.to_owned()))
        }
        [.., Token::Identifier(identifier), Token::Operator(Operator::Minus)] => {
            (2, Some(Occurrence::Forbidden), Some(identifier.to_owned()))
        }
        [.., Token::Operator(Operator::Plus)] => (1, Some(Occurrence::Required), None),
        [.., Token::Operator(Operator::Minus)] => (1, Some(Occurrence::Forbidden), None),
        [.., Token::Identifier(identifier)] => (1, None, Some(identifier.to_owned())),
        _ => (0, None, None),
    };

    // Pop of tokens that have been read:
    for _ in 0..num {
        tokens.pop();
    }

    // TODO: lookup field with identifier to get kind so it can be passed on to perform type inference and checks.

    Ok(Clause {
        occurrence,
        field: todo!(),
        body: todo!(),
    })
}

fn parse_clauses<'a, 'b>(tokens: &mut Vec<Token<'a>>) -> Result<Vec<Clause>, ParserError<'a>> {
    if tokens.is_empty() {
        return Ok(Vec::new());
    }

    let mut clauses = vec![parse_clause(tokens)?];
    clauses.append(&mut parse_clauses(tokens)?);

    Ok(clauses)
}

fn parse<'a>(mut tokens: Vec<Token<'a>>) -> Result<Expression, ParserError<'a>> {
    // Reverse the tokens to turn it into a stack:
    tokens.reverse();

    let clauses = parse_clauses(&mut tokens)?;

    Ok(Expression(clauses))
}
