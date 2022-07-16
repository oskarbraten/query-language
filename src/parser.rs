use std::{
    cmp::Ordering,
    collections::BTreeSet,
    num::{ParseFloatError, ParseIntError},
    ops::Bound,
};
use time::{
    error::Parse as DateTimeParseError,
    format_description::well_known::{Iso8601, Rfc3339},
    OffsetDateTime,
};

use crate::lexer::{Lexer, *};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Occurrence {
    Required,
    Forbidden,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    String(String),
    Float(f64),
    UInt(u64),
    Int(i64),
    Date(OffsetDateTime),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum FieldKind {
    String,
    Float,
    UInt,
    Int,
    Date,
}

#[derive(Debug, Clone, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Field {
    pub name: String,
    pub kind: FieldKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // Terminals:
    Regex(String),
    Phrase(String),
    Range(Bound<Primitive>, Bound<Primitive>),
    Term(Primitive, Option<u8>),

    // Modifiers:
    Boost(f64, Box<Expression>),
    Occurrence(Occurrence, Box<Expression>),
    Field(Field, Box<Expression>),

    Clause(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnexpectedToken,
    ExpectedFieldKind(FieldKind),
    NoSuchField { alternatives: Vec<Field> },
    InvalidFloat(ParseFloatError),
    InvalidInt(ParseIntError),
    InvalidDate(DateTimeParseError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub current: Option<Token>,
    pub kind: ErrorKind,
}

struct Parser {
    lexer: Lexer,
    fields: BTreeSet<Field>,
}

impl Parser {
    fn primitive(
        &self,
        value: &str,
        kind: FieldKind,
        token: Token,
    ) -> Result<Primitive, ParseError> {
        match kind {
            FieldKind::String { .. } => Ok(Primitive::String(value.to_string())),
            FieldKind::Float => value
                .parse()
                .map_err(|error| ErrorKind::InvalidFloat(error))
                .map(|value| Primitive::Float(value)),
            FieldKind::Int => value
                .parse()
                .map_err(|error| ErrorKind::InvalidInt(error))
                .map(|value| Primitive::Int(value)),
            FieldKind::UInt => value
                .parse()
                .map_err(|error| ErrorKind::InvalidInt(error))
                .map(|value| Primitive::UInt(value)),
            FieldKind::Date => OffsetDateTime::parse(value, &Rfc3339)
                .or_else(|_| OffsetDateTime::parse(value, &Iso8601::PARSING))
                .map_err(|error| ErrorKind::InvalidDate(error))
                .map(|value| Primitive::Date(value)),
        }
        .map_err(|kind| ParseError {
            current: Some(token),
            kind,
        })
    }

    fn bound(
        &self,
        bound: Bound<Span>,
        kind: FieldKind,
        token: Token,
    ) -> Result<Bound<Primitive>, ParseError> {
        match &bound {
            Bound::Included(span) | Bound::Excluded(span) => {
                let primitive = self.primitive(span, kind, token)?;

                if let Bound::Included(_) = bound {
                    Ok(Bound::Included(primitive))
                } else {
                    Ok(Bound::Excluded(primitive))
                }
            }
            Bound::Unbounded => Ok(Bound::Unbounded),
        }
    }

    fn literal(&self, token: &Token, kind: FieldKind) -> Result<Expression, ParseError> {
        match token {
            Token::Literal(Literal::Term(value, distance)) => Ok(Expression::Term(
                self.primitive(&value, kind, token.clone())?,
                distance.clone(),
            )),
            Token::Literal(Literal::Phrase(Span { value, .. })) => match kind {
                FieldKind::String => Ok(Expression::Phrase(value.to_string())),
                _ => {
                    return Err(ParseError {
                        current: Some(token.clone()),
                        kind: ErrorKind::ExpectedFieldKind(FieldKind::String),
                    });
                }
            },
            Token::Literal(Literal::Range(from, to)) => Ok(Expression::Range(
                self.bound(from.clone(), kind, token.clone())?,
                self.bound(to.clone(), kind, token.clone())?,
            )),
            Token::Literal(Literal::Regex(Span { value, .. })) => {
                Ok(Expression::Regex(value.to_string()))
            }
            _ => Err(ParseError {
                current: Some(token.clone()),
                kind: ErrorKind::UnexpectedToken,
            }),
        }
    }

    fn field(&self, token: &Token) -> Result<Field, ParseError> {
        match token {
            Token::Identifier(identifier) => {
                let identifier: &str = &identifier;

                match self
                    .fields
                    .iter()
                    .find(|Field { name, .. }| name == identifier)
                {
                    Some(field) => Ok(field.clone()),
                    _ => {
                        let mut fields: Vec<(f64, Field)> = self
                            .fields
                            .iter()
                            .cloned()
                            .map(|field| (strsim::jaro_winkler(&field.name, identifier), field))
                            .collect();

                        fields.sort_by(|(s1, _), (s2, _)| {
                            s2.partial_cmp(s1).unwrap_or(Ordering::Less)
                        });

                        Err(ParseError {
                            current: Some(token.clone()),
                            kind: ErrorKind::NoSuchField {
                                alternatives: fields
                                    .into_iter()
                                    .take(5)
                                    .map(|(_, field)| field)
                                    .collect(),
                            },
                        })
                    }
                }
            }
            _ => Err(ParseError {
                current: Some(token.clone()),
                kind: ErrorKind::UnexpectedToken,
            }),
        }
    }

    fn infix_binding_power(operator: Operator) -> Option<(u8, u8)> {
        match operator {
            Operator::Or => Some((1, 2)),
            Operator::And => Some((3, 4)),
            _ => None,
        }
    }

    fn prefix_binding_power(operator: Operator) -> ((), u8) {
        match operator {
            Operator::Plus | Operator::Minus | Operator::Not => ((), 5),
            _ => panic!("Bad operator: {operator:?}!"),
        }
    }

    fn postfix_binding_power(operator: Operator) -> Option<(u8, ())> {
        match operator {
            Operator::Boost => Some((7, ())),
            _ => None,
        }
    }

    fn expression(&mut self, min_bp: u8, kind: FieldKind) -> Result<Expression, ParseError> {
        let mut lhs = match self.lexer.next() {
            ref token @ Token::Literal(_) => self.literal(token, kind)?,
            ref token @ Token::Identifier(_) => {
                let field = self.field(token)?;
                let rhs = self.expression(u8::MAX, field.kind)?;

                Expression::Field(field, Box::new(rhs))
            }
            Token::Operator(operator @ (Operator::Plus | Operator::Minus | Operator::Not)) => {
                let ((), r_bp) = Self::prefix_binding_power(operator);

                let rhs = self.expression(r_bp, kind)?;
                Expression::Occurrence(
                    match operator {
                        Operator::Minus | Operator::Not => Occurrence::Forbidden,
                        Operator::Plus => Occurrence::Required,
                        _ => unreachable!("outer match excludes other variants"),
                    },
                    Box::new(rhs),
                )
            }
            Token::Punctuation(Punctuation::ParenOpen) => {
                let expression = self.expression(0, kind)?;
                assert_eq!(
                    self.lexer.next(),
                    Token::Punctuation(Punctuation::ParenClose)
                );

                expression
            }
            token => {
                return Err(ParseError {
                    current: Some(token.clone()),
                    kind: ErrorKind::UnexpectedToken,
                })
            }
        };

        loop {
            let operator = match self.lexer.peek() {
                Token::Punctuation(Punctuation::ParenClose) | Token::Eof => break,
                Token::Operator(operator) => operator,
                token => panic!("Bad token: {token:?}"),
            };

            if let Some((l_bp, ())) = Self::postfix_binding_power(operator) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                match operator {
                    Operator::Boost => {
                        let token = self.lexer.next();
                        let literal = self.literal(&token, FieldKind::Float)?;

                        let boost = match literal {
                            Expression::Term(Primitive::Float(value), None) => value,
                            _ => {
                                return Err(ParseError {
                                    current: Some(token),
                                    kind: ErrorKind::UnexpectedToken,
                                })
                            }
                        };

                        lhs = Expression::Boost(boost, Box::new(lhs));
                    }
                    token => panic!("Bad operator: {token:?}"),
                }

                continue;
            }

            if let Some((l_bp, r_bp)) = Self::infix_binding_power(operator) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                let rhs = self.expression(r_bp, kind)?;

                match operator {
                    Operator::And => {
                        lhs = Expression::Clause(vec![
                            Expression::Occurrence(Occurrence::Required, Box::new(lhs)),
                            Expression::Occurrence(Occurrence::Required, Box::new(rhs)),
                        ]);
                    }
                    Operator::Or => {
                        lhs = Expression::Clause(vec![lhs, rhs]);
                    }
                    token => panic!("Bad operator: {token:?}"),
                }

                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

// fn optimize(expression: Expression) -> Expression {
//     match expression {
//         Expression::Clause(exprs) => {
//             let flatten = exprs.iter().all(|expr| match expr {
//                 Expression::Clause(exprs) => exprs.iter().all(|expr| match expr {
//                     Expression::Occurrence(Occurrence::Required, _) => false,
//                     _ => true,
//                 }),
//                 _ => true,
//             });

//             if flatten {
//                 let exprs = exprs
//                     .into_iter()
//                     .flat_map(|expr| match expr {
//                         Expression::Clause(exprs) => exprs,
//                         _ => vec![expr],
//                     })
//                     .collect();

//                 optimize(Expression::Clause(exprs))
//             } else {
//                 Expression::Clause(exprs)
//             }
//         }
//         _ => expression,
//     }
// }

pub fn parse(
    lexer: Lexer,
    fields: BTreeSet<Field>,
    default_kind: FieldKind,
) -> Result<Expression, ParseError> {
    let mut parser = Parser { lexer, fields };
    parser.expression(0, default_kind)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fields() -> BTreeSet<Field> {
        let mut fields = BTreeSet::new();

        fields.insert(Field {
            name: String::from("name"),
            kind: FieldKind::String,
        });
        fields.insert(Field {
            name: String::from("memes"),
            kind: FieldKind::String,
        });
        fields.insert(Field {
            name: String::from("perceived_date"),
            kind: FieldKind::Date,
        });
        fields.insert(Field {
            name: String::from("received_date"),
            kind: FieldKind::Date,
        });

        fields
    }

    #[test]
    fn simple() {
        let lexer = Lexer::new(
            "name:bar^23 AND test || \"Hello, world!\" || NOT test3 && perceived_date:[1970-01-01T00:00:00Z..]",
        )
        .unwrap();

        assert_eq!(
            parse(lexer, fields(), FieldKind::String),
            Ok(Expression::Clause(vec![
                Expression::Clause(vec![
                    Expression::Clause(vec![
                        Expression::Occurrence(
                            Occurrence::Required,
                            Box::new(Expression::Boost(
                                23.0,
                                Box::new(Expression::Field(
                                    Field {
                                        name: String::from("name"),
                                        kind: FieldKind::String,
                                    },
                                    Box::new(Expression::Term(
                                        Primitive::String(String::from("bar")),
                                        None
                                    )),
                                )),
                            )),
                        ),
                        Expression::Occurrence(
                            Occurrence::Required,
                            Box::new(Expression::Term(
                                Primitive::String(String::from("test")),
                                None
                            )),
                        ),
                    ],),
                    Expression::Phrase(String::from("Hello, world!"))
                ]),
                Expression::Clause(vec![
                    Expression::Occurrence(
                        Occurrence::Required,
                        Box::new(Expression::Occurrence(
                            Occurrence::Forbidden,
                            Box::new(Expression::Term(
                                Primitive::String(String::from("test3")),
                                None
                            ))
                        ))
                    ),
                    Expression::Occurrence(
                        Occurrence::Required,
                        Box::new(Expression::Field(
                            Field {
                                name: String::from("perceived_date"),
                                kind: FieldKind::Date,
                            },
                            Box::new(Expression::Range(
                                Bound::Included(Primitive::Date(OffsetDateTime::UNIX_EPOCH)),
                                Bound::Unbounded
                            ))
                        ))
                    )
                ])
            ]))
        );
    }
}
