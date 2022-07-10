use std::ops::Bound;

mod regex;
use self::regex::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    Colon,
    ParensOpen,
    ParensClose,
    BracketOpen,
    BracketClose,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    Phrase(&'a str),
    Regex(&'a str),
    Range(Bound<&'a str>, Bound<&'a str>),
    Term(&'a str, Option<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Literal(Literal<'a>),
    Operator(Operator),
    Punctuation(Punctuation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexerError<'a> {
    pub remaining: &'a str,
    pub preceding: Option<Token<'a>>,
}

fn analyze_recursive<'a>(text: &'a str, tokens: &mut Vec<Token<'a>>) -> Result<(), LexerError<'a>> {
    if text.is_empty() {
        return Ok(());
    }

    if let Some(end) = REGEX_WHITESPACE.find(text).map(|m| m.end()) {
        return analyze_recursive(&text[end..], tokens);
    }

    if let Some(end) = REGEX_IDENTIFIER.find(text).map(|m| m.end()) {
        tokens.push(Token::Identifier(&text[..end - 1]));
        return analyze_recursive(&text[end - 1..], tokens);
    }

    if REGEX_PUNCTUATION_COLON.is_match(text) {
        tokens.push(Token::Punctuation(Punctuation::Colon));
        return analyze_recursive(&text[1..], tokens);
    } else if REGEX_PUNCTUATION_PARENS_OPEN.is_match(text) {
        tokens.push(Token::Punctuation(Punctuation::ParensOpen));
        return analyze_recursive(&text[1..], tokens);
    } else if REGEX_PUNCTUATION_PARENS_CLOSE.is_match(text) {
        tokens.push(Token::Punctuation(Punctuation::ParensClose));
        return analyze_recursive(&text[1..], tokens);
    }

    if REGEX_OPERATOR_AND.is_match(text) {
        tokens.push(Token::Operator(Operator::And));
        return analyze_recursive(&text[3..], tokens);
    } else if REGEX_OPERATOR_OR.is_match(text) {
        tokens.push(Token::Operator(Operator::Or));
        return analyze_recursive(&text[2..], tokens);
    } else if REGEX_OPERATOR_PLUS.is_match(text) {
        tokens.push(Token::Operator(Operator::Plus));
        return analyze_recursive(&text[1..], tokens);
    } else if REGEX_OPERATOR_MINUS.is_match(text) {
        tokens.push(Token::Operator(Operator::Minus));
        return analyze_recursive(&text[1..], tokens);
    }

    if let Some(captures) = REGEX_LITERAL_PHRASE.captures(text) {
        let phrase: &'a str = captures.name("phrase").unwrap().as_str();
        tokens.push(Token::Literal(Literal::Phrase(phrase)));

        let end = captures.name("terminal").unwrap().start();
        return analyze_recursive(&text[end..], tokens);
    }

    if let Some(captures) = REGEX_LITERAL_REGEX.captures(text) {
        let regex: &'a str = captures.name("regex").unwrap().as_str();
        tokens.push(Token::Literal(Literal::Regex(regex)));

        let end = captures.name("terminal").unwrap().start();
        return analyze_recursive(&text[end..], tokens);
    }

    if let Some(captures) = REGEX_LITERAL_RANGE.captures(text) {
        let token = {
            let start: Bound<&'a str> = captures
                .name("start")
                .filter(|m| !m.as_str().is_empty())
                .map(|m| Bound::Included(m.as_str()))
                .unwrap_or(Bound::Unbounded);

            let inclusive = captures
                .name("inclusive")
                .filter(|m| !m.as_str().is_empty())
                .is_some();
            let end: Bound<&'a str> = captures
                .name("end")
                .filter(|m| !m.as_str().is_empty())
                .map(|m| {
                    if inclusive {
                        Bound::Included(m.as_str())
                    } else {
                        Bound::Excluded(m.as_str())
                    }
                })
                .unwrap_or(Bound::Unbounded);

            Token::Literal(Literal::Range(start, end))
        };

        tokens.push(token);

        let end = captures.name("terminal").unwrap().start();
        return analyze_recursive(&text[end..], tokens);
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
            tokens.push(Token::Literal(Literal::Term(term, Some(distance))));
        } else {
            let term: &'a str = captures.name("term").unwrap().as_str();
            tokens.push(Token::Literal(Literal::Term(term, None)));
        };

        let end = captures.name("terminal").unwrap().start();
        return analyze_recursive(&text[end..], tokens);
    }

    Err(LexerError {
        remaining: text,
        preceding: tokens.pop(),
    })
}

pub fn analyze<'a>(text: &'a str) -> Result<Vec<Token<'a>>, LexerError<'a>> {
    let mut tokens = Vec::new();
    analyze_recursive(text, &mut tokens).map(|_| tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn terms() {
        assert_eq!(
            analyze("hello world"),
            Ok(vec![
                Token::Literal(Literal::Term("hello", None)),
                Token::Literal(Literal::Term("world", None))
            ])
        );

        assert_eq!(
            analyze("hello AND world"),
            Ok(vec![
                Token::Literal(Literal::Term("hello", None)),
                Token::Operator(Operator::And),
                Token::Literal(Literal::Term("world", None))
            ])
        );

        assert_eq!(
            analyze("hello OR world"),
            Ok(vec![
                Token::Literal(Literal::Term("hello", None)),
                Token::Operator(Operator::Or),
                Token::Literal(Literal::Term("world", None))
            ])
        );

        assert_eq!(
            analyze("f1:hello f2:world"),
            Ok(vec![
                Token::Identifier("f1"),
                Token::Punctuation(Punctuation::Colon),
                Token::Literal(Literal::Term("hello", None)),
                Token::Identifier("f2"),
                Token::Punctuation(Punctuation::Colon),
                Token::Literal(Literal::Term("world", None))
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
        let query = r###"f1:hello AND f2:"Hello, world!" OR f3:(rofl +cool -beans)"###;

        let tokens = vec![
            Token::Identifier("f1"),
            Token::Punctuation(Punctuation::Colon),
            Token::Literal(Literal::Term("hello", None)),
            Token::Operator(Operator::And),
            Token::Identifier("f2"),
            Token::Punctuation(Punctuation::Colon),
            Token::Literal(Literal::Phrase("Hello, world!")),
            Token::Operator(Operator::Or),
            Token::Identifier("f3"),
            Token::Punctuation(Punctuation::Colon),
            Token::Punctuation(Punctuation::ParensOpen),
            Token::Literal(Literal::Term("rofl", None)),
            Token::Operator(Operator::Plus),
            Token::Literal(Literal::Term("cool", None)),
            Token::Operator(Operator::Minus),
            Token::Literal(Literal::Term("beans", None)),
            Token::Punctuation(Punctuation::ParensClose),
        ];

        assert_eq!(analyze(query).unwrap(), tokens);
    }
}
