use std::ops::Bound;

mod regex;
use self::regex::*;

mod token;
pub use token::*;

#[derive(Debug, Clone, PartialEq)]
pub struct LexerError<'a> {
    pub remaining: &'a str,
    pub preceding: Vec<Token>,
}

pub struct Lexer {
    tokens: Vec<Token>,
    offset: usize,
}

impl Lexer {
    pub fn new<'a>(source: &'a str) -> Result<Self, LexerError<'a>> {
        let mut lexer = Self {
            tokens: Vec::new(),
            offset: source.as_ptr() as usize,
        };

        lexer.analyze_recursive(source)?;
        lexer.tokens.reverse();

        Ok(lexer)
    }

    fn span(&self, value: &str) -> Span {
        Span::new(value, self.offset)
    }

    fn analyze_recursive<'a>(&mut self, text: &'a str) -> Result<(), LexerError<'a>> {
        if text.is_empty() {
            return Ok(());
        }

        if let Some(end) = REGEX_WHITESPACE.find(text).map(|m| m.end()) {
            return self.analyze_recursive(&text[end..]);
        }

        if let Some(captures) = REGEX_IDENTIFIER.captures(text) {
            let identifier: &'a str = captures.name("identifier").unwrap().as_str();
            self.tokens.push(Token::Identifier(self.span(identifier)));

            let end = captures[0].len();
            return self.analyze_recursive(&text[end..]);
        }

        if REGEX_PUNCTUATION_PARENS_OPEN.is_match(text) {
            self.tokens.push(Token::Punctuation(Punctuation::ParenOpen));
            return self.analyze_recursive(&text[1..]);
        } else if REGEX_PUNCTUATION_PARENS_CLOSE.is_match(text) {
            self.tokens
                .push(Token::Punctuation(Punctuation::ParenClose));
            return self.analyze_recursive(&text[1..]);
        }

        if REGEX_OPERATOR_AND.is_match(text) {
            self.tokens.push(Token::Operator(Operator::And));
            return self.analyze_recursive(&text[3..]);
        } else if REGEX_OPERATOR_OR.is_match(text) {
            self.tokens.push(Token::Operator(Operator::Or));
            return self.analyze_recursive(&text[2..]);
        } else if REGEX_OPERATOR_NOT.is_match(text) {
            self.tokens.push(Token::Operator(Operator::Not));
            return self.analyze_recursive(&text[3..]);
        } else if REGEX_OPERATOR_PLUS.is_match(text) {
            self.tokens.push(Token::Operator(Operator::Plus));
            return self.analyze_recursive(&text[1..]);
        } else if REGEX_OPERATOR_MINUS.is_match(text) {
            self.tokens.push(Token::Operator(Operator::Minus));
            return self.analyze_recursive(&text[1..]);
        } else if REGEX_OPERATOR_BOOST.is_match(text) {
            self.tokens.push(Token::Operator(Operator::Boost));
            return self.analyze_recursive(&text[1..]);
        }

        if let Some(captures) = REGEX_LITERAL_PHRASE.captures(text) {
            let phrase: &'a str = captures.name("phrase").unwrap().as_str();
            self.tokens
                .push(Token::Literal(Literal::Phrase(self.span(phrase))));

            let end = captures.name("terminal").unwrap().start();
            return self.analyze_recursive(&text[end..]);
        }

        if let Some(captures) = REGEX_LITERAL_REGEX.captures(text) {
            let regex: &'a str = captures.name("regex").unwrap().as_str();
            self.tokens
                .push(Token::Literal(Literal::Regex(self.span(regex))));

            let end = captures.name("terminal").unwrap().start();
            return self.analyze_recursive(&text[end..]);
        }

        if let Some(captures) = REGEX_LITERAL_RANGE.captures(text) {
            let token = {
                let start: Bound<Span> = captures
                    .name("start")
                    .filter(|m| !m.as_str().is_empty())
                    .map(|m| Bound::Included(self.span(m.as_str())))
                    .unwrap_or(Bound::Unbounded);

                let inclusive = captures
                    .name("inclusive")
                    .filter(|m| !m.as_str().is_empty())
                    .is_some();
                let end: Bound<Span> = captures
                    .name("end")
                    .filter(|m| !m.as_str().is_empty())
                    .map(|m| {
                        if inclusive {
                            Bound::Included(self.span(m.as_str()))
                        } else {
                            Bound::Excluded(self.span(m.as_str()))
                        }
                    })
                    .unwrap_or(Bound::Unbounded);

                Token::Literal(Literal::Range(start, end))
            };

            self.tokens.push(token);

            let end = captures.name("terminal").unwrap().start();
            return self.analyze_recursive(&text[end..]);
        }

        if let Some(captures) = REGEX_LITERAL_TERM.captures(text) {
            let fuzzy_term = captures.name("fterm").map(|m| {
                let term: &'a str = m.as_str();

                (
                    term,
                    captures
                        .name("distance")
                        .filter(|m| !m.as_str().is_empty())
                        .map(|m| m.as_str().parse::<u8>().unwrap())
                        .unwrap_or(1),
                )
            });

            if let Some((term, distance)) = fuzzy_term {
                self.tokens.push(Token::Literal(Literal::Term(
                    self.span(term),
                    Some(distance),
                )));
            } else {
                let term: &'a str = captures.name("term").unwrap().as_str();
                self.tokens
                    .push(Token::Literal(Literal::Term(self.span(term), None)));
            };

            let end = captures.name("terminal").unwrap().start();
            return self.analyze_recursive(&text[end..]);
        };

        Err(LexerError {
            remaining: text,
            preceding: self.tokens.drain(..).collect(),
        })
    }

    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    pub fn peek(&mut self) -> Token {
        self.tokens.last().cloned().unwrap_or(Token::Eof)
    }

    pub fn collect(self) -> Vec<Token> {
        self.tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        let source = "hello world";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span::new(&source[6..], source.as_ptr() as usize),
                    None
                )),
                Token::Literal(Literal::Term(
                    Span::new(&source[..5], source.as_ptr() as usize),
                    None
                )),
            ])
        );
    }

    #[test]
    fn hello_and_world() {
        let source = "hello AND world";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span::new(&source[10..], source.as_ptr() as usize),
                    None
                )),
                Token::Operator(Operator::And),
                Token::Literal(Literal::Term(
                    Span::new(&source[..5], source.as_ptr() as usize),
                    None
                )),
            ])
        );
    }

    #[test]
    fn hello_or_world() {
        let source = "hello OR world";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span::new(&source[9..], source.as_ptr() as usize),
                    None
                )),
                Token::Operator(Operator::Or),
                Token::Literal(Literal::Term(
                    Span::new(&source[..5], source.as_ptr() as usize),
                    None
                )),
            ])
        );
    }

    #[test]
    fn f1_hello_f2_world() {
        let source = "f1:hello f2:world";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span::new(&source[12..], source.as_ptr() as usize),
                    None
                )),
                Token::Identifier(Span::new(&source[9..11], source.as_ptr() as usize)),
                Token::Literal(Literal::Term(
                    Span::new(&source[3..8], source.as_ptr() as usize),
                    None
                )),
                Token::Identifier(Span::new(&source[..2], source.as_ptr() as usize)),
            ])
        );
    }

    #[test]
    fn boost1() {
        let source = "hello^2.3";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("2.3"),
                        start: 6,
                        end: 9
                    },
                    None
                )),
                Token::Operator(Operator::Boost),
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("hello"),
                        start: 0,
                        end: 5
                    },
                    None
                ))
            ])
        );
    }

    #[test]
    fn boost2() {
        let source = "tester hello^2.3foo";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("2.3foo"),
                        start: 13,
                        end: 19
                    },
                    None
                )),
                Token::Operator(Operator::Boost),
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("hello"),
                        start: 7,
                        end: 12
                    },
                    None
                )),
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("tester"),
                        start: 0,
                        end: 6
                    },
                    None
                ))
            ])
        );
    }

    #[test]
    fn boost3() {
        let source = "hello^2.3 foobar";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("foobar"),
                        start: 10,
                        end: 16
                    },
                    None
                )),
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("2.3"),
                        start: 6,
                        end: 9
                    },
                    None
                )),
                Token::Operator(Operator::Boost),
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("hello"),
                        start: 0,
                        end: 5
                    },
                    None
                ))
            ])
        );
    }

    #[test]
    fn date1() {
        let source = "not-a-date 1990-12-31T23:59:60Z";
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("1990-12-31T23:59:60Z"),
                        start: 11,
                        end: 31
                    },
                    None
                )),
                Token::Literal(Literal::Term(
                    Span {
                        value: String::from("not-a-date"),
                        start: 0,
                        end: 10
                    },
                    None
                ))
            ])
        );
    }

    #[test]
    fn phrase() {
        todo!()
    }

    #[test]
    fn regex() {
        todo!()
    }

    #[test]
    fn range() {
        todo!()
    }

    #[test]
    fn simple_analyze() {
        let source = r###"f1:hello AND f2:"Hello, world!" OR f3:(rofl +cool -beans)"###;
        assert_eq!(
            Lexer::new(source).map(|lexer| lexer.collect()),
            Ok(vec![
                Token::Punctuation(Punctuation::ParenClose),
                Token::Literal(Literal::Term(
                    Span {
                        value: "beans".to_string(),
                        start: 51,
                        end: 56
                    },
                    None
                )),
                Token::Operator(Operator::Minus),
                Token::Literal(Literal::Term(
                    Span {
                        value: "cool".to_string(),
                        start: 45,
                        end: 49
                    },
                    None
                )),
                Token::Operator(Operator::Plus),
                Token::Literal(Literal::Term(
                    Span {
                        value: "rofl".to_string(),
                        start: 39,
                        end: 43
                    },
                    None
                )),
                Token::Punctuation(Punctuation::ParenOpen),
                Token::Identifier(Span {
                    value: "f3".to_string(),
                    start: 35,
                    end: 37
                }),
                Token::Operator(Operator::Or),
                Token::Literal(Literal::Phrase(Span {
                    value: "Hello, world!".to_string(),
                    start: 17,
                    end: 30
                })),
                Token::Identifier(Span {
                    value: "f2".to_string(),
                    start: 13,
                    end: 15
                }),
                Token::Operator(Operator::And),
                Token::Literal(Literal::Term(
                    Span {
                        value: "hello".to_string(),
                        start: 3,
                        end: 8
                    },
                    None
                )),
                Token::Identifier(Span {
                    value: "f1".to_string(),
                    start: 0,
                    end: 2
                })
            ])
        );
    }
}
