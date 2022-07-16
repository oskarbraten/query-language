use lazy_static::lazy_static;
use regex::Regex;

const REGEX_LITERAL_TERMINAL: &'static str = r"(?x)
    (?P<terminal>
        \^| # Boost operator
        \)| # Closing parenthesis
        \s| # Whitespace
        $ # End of input
    )";

const REGEX_LITERAL_TERM_CHARACTER: &'static str = r"(\\\)|\\\^|[^\s)\^])";

lazy_static! {
    pub static ref REGEX_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();

    // Identifier:
    pub static ref REGEX_IDENTIFIER: Regex = Regex::new(r"^(?P<identifier>[A-Za-z][A-Za-z0-9_]*):").unwrap();

    // Operators:
    pub static ref REGEX_OPERATOR_AND: Regex = Regex::new(r"^(AND|&&)\s").unwrap();
    pub static ref REGEX_OPERATOR_OR: Regex = Regex::new(r"^(OR|\|\|)\s").unwrap();
    pub static ref REGEX_OPERATOR_NOT: Regex = Regex::new(r"^(NOT)\s").unwrap();
    pub static ref REGEX_OPERATOR_PLUS: Regex = Regex::new(r"^\+\S").unwrap();
    pub static ref REGEX_OPERATOR_MINUS: Regex = Regex::new(r"^\-\S").unwrap();
    pub static ref REGEX_OPERATOR_BOOST: Regex = Regex::new(r"^\^").unwrap();

    // Punctuation:
    pub static ref REGEX_PUNCTUATION_PARENS_OPEN: Regex = Regex::new(r"^\(").unwrap();
    pub static ref REGEX_PUNCTUATION_PARENS_CLOSE: Regex = Regex::new(r"^\)").unwrap();

    // Literals:
    pub static ref REGEX_LITERAL_PHRASE: Regex = Regex::new(&format!(r#"(?x)
    ^"(?P<phrase>
        (\\"|[^"])* # Match an escaped quote \" or any character that is not a quote.
    )"
    {REGEX_LITERAL_TERMINAL}
    "#)).unwrap();

    pub static ref REGEX_LITERAL_REGEX: Regex = Regex::new(&format!(r"(?x)
    ^(?:/
        (?P<regex>(?:[^\r\n\[/\\]|\\.|\[(?:[^\r\n\]\\]|\\.)*\])*)
    /)
    {REGEX_LITERAL_TERMINAL}
    ")).unwrap();

    pub static ref REGEX_LITERAL_RANGE: Regex = Regex::new(&format!(r"(?x)
    ^(?:\[
        (?P<start>\S*) # Start term
        \.\.(?P<inclusive>=?) # Inclusive (optional)
        (?P<end>\S*) # End term
    \])
    {REGEX_LITERAL_TERMINAL}
    ")).unwrap();

    // Term - Optional fuzzy matching from group 4 and 5 (with value in 3)
    pub static ref REGEX_LITERAL_TERM: Regex = Regex::new(&format!(r"(?x)
    ^(?:
        (?:(?P<fterm>{REGEX_LITERAL_TERM_CHARACTER}+)(?:~(?P<distance>\d*)))| # matches a term with fuzzy suffix (e.g. term1~, term2~3, ...), or
        (?P<term>{REGEX_LITERAL_TERM_CHARACTER}+) # matches a term without fuzzy suffix (e.g. term1, term2)
    )
    {REGEX_LITERAL_TERMINAL}
    ")).unwrap();
}
