use std::rc::Rc;

use crate::source::Source;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    Slash,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    Equal,
    EqualEqual,
    Bang,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    SemiColon,
    Colon,
    Comma,
    Dot,

    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,

    // Literal types
    Number,
    String,
    True,
    False,
    None,

    If,
    In,
    Else,
    For,
    While,
    Include,
    Switch,
    Function,
    Return,
    Static,
    Identifier,
    And,
    Or,
    Let,
    Mutable,
    UnderscoreUnderscore,
    Module,
    Struct,
    This,
    Loop,
    Yield,
    Resume,

    EndOfFile,
    Error,
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub len: u16,
    pub source: Rc<Source>,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
    pub column: u16,
    pub lexeme: Option<Span>,
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        format!("{self:?}")
    }
}

impl Span {
    pub fn get_slice(&self) -> &str {
        self.source.slice_from(self.clone()).unwrap()
    }

    pub fn compare(&self, other: &Span) -> bool {
        let lhs = self.source.slice_from(self.clone()).unwrap();
        let rhs = other.source.slice_from(other.clone()).unwrap();

        lhs == rhs
    }
}
