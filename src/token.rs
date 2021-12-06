use std::{
    fmt::{Debug, Write},
    hash::Hasher,
    ops::Deref,
    rc::Rc,
};

use sha::{sha1::Sha1, utils::Digest};

macro_rules! asm {
    ($op:expr, $($arg:expr),*) => {
        Asm::Op {
            op: $op.to_string(),
            args: vec![$($arg.to_string()),*],
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    /// Assignment operation `=`
    Assign(),
    /// Addition operation `+`
    Add(),
    /// Addition & Assignment operation `+=`
    AddAssign(),
    Sub(),
    SubAssign(),
    Mul(),
    MulAssign(),
    Div(),
    DivAssign(),
    Mod(),
    ModAssign(),
    /// Line ending `;`
    LineEnd(),
    Not(),
    And(),
    Or(),
    Xor(),
    Dot(),
    Comma(),
    Colon(),
    Greater(),
    Less(),

    AndBool(),
    OrBool(),
    Equal(),
    NotEqual(),
    GreaterEq(),
    LessEq(),
    Namespace(),
}

impl Operation {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Self::Add()),
            '-' => Some(Self::Sub()),
            '*' => Some(Self::Mul()),
            '/' => Some(Self::Div()),
            '%' => Some(Self::Mod()),
            '=' => Some(Self::Assign()),
            ';' => Some(Self::LineEnd()),
            '!' => Some(Self::Not()),
            '&' => Some(Self::And()),
            '|' => Some(Self::Or()),
            '^' => Some(Self::Xor()),
            '.' => Some(Self::Dot()),
            ',' => Some(Self::Comma()),
            ':' => Some(Self::Colon()),
            '>' => Some(Self::Greater()),
            '<' => Some(Self::Less()),
            _ => None,
        }
    }

    pub fn from_pair(a: &Self, b: &Self) -> Option<Self> {
        match (a, b) {
            (Self::Add(), Self::Assign()) => Some(Self::AddAssign()),
            (Self::Sub(), Self::Assign()) => Some(Self::SubAssign()),
            (Self::Mul(), Self::Assign()) => Some(Self::MulAssign()),
            (Self::Div(), Self::Assign()) => Some(Self::DivAssign()),
            (Self::Mod(), Self::Assign()) => Some(Self::ModAssign()),
            (Self::And(), Self::And()) => Some(Self::AndBool()),
            (Self::Or(), Self::Or()) => Some(Self::OrBool()),
            (Self::Assign(), Self::Assign()) => Some(Self::Equal()),
            (Self::Not(), Self::Assign()) => Some(Self::NotEqual()),
            (Self::Less(), Self::Assign()) => Some(Self::LessEq()),
            (Self::Greater(), Self::Assign()) => Some(Self::GreaterEq()),
            (Self::Colon(), Self::Colon()) => Some(Self::Namespace()),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Assign() => todo!(),
            Self::Add() => "add",
            Self::AddAssign() => todo!(),
            Self::Sub() => "sub",
            Self::SubAssign() => todo!(),
            Self::Mul() => "mul",
            Self::MulAssign() => todo!(),
            Self::Div() => "div",
            Self::DivAssign() => todo!(),
            Self::Mod() => "mod",
            Self::ModAssign() => todo!(),
            Self::LineEnd() => todo!(),
            Self::Not() => "not",
            Self::And() => "bitand",
            Self::Or() => "bitor",
            Self::Xor() => "xor",
            Self::Dot() => "dot",
            Self::Comma() => todo!(),
            Self::Colon() => todo!(),
            Self::Greater() => todo!(),
            Self::Less() => todo!(),
            Self::AndBool() => todo!(),
            Self::OrBool() => todo!(),
            Self::Equal() => todo!(),
            Self::NotEqual() => todo!(),
            Self::GreaterEq() => todo!(),
            Self::LessEq() => todo!(),
            Self::Namespace() => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Group {
    OpenParen(),
    OpenCurly(),
    OpenAngle(),
    OpenSquare(),
    CloseParen(),
    CloseCurly(),
    CloseAngle(),
    CloseSquare(),
}

impl Group {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '(' => Some(Self::OpenParen()),
            ')' => Some(Self::CloseParen()),
            '{' => Some(Self::OpenCurly()),
            '}' => Some(Self::CloseCurly()),
            //'<' => Some(Self::OpenAngle()),
            //'>' => Some(Self::CloseAngle()),
            '[' => Some(Self::OpenSquare()),
            ']' => Some(Self::CloseSquare()),
            _ => None,
        }
    }

    pub fn close(&self) -> Self {
        match self {
            Self::OpenParen() => Self::CloseParen(),
            Self::OpenCurly() => Self::CloseCurly(),
            Self::OpenAngle() => Self::CloseAngle(),
            Self::OpenSquare() => Self::CloseSquare(),
            Self::CloseParen() => Self::CloseParen(),
            Self::CloseCurly() => Self::CloseCurly(),
            Self::CloseAngle() => Self::CloseAngle(),
            Self::CloseSquare() => Self::CloseSquare(),
        }
    }

    pub fn is_open(&self) -> bool {
        match self {
            Self::OpenParen() => true,
            Self::OpenCurly() => true,
            Self::OpenAngle() => true,
            Self::OpenSquare() => true,
            Self::CloseParen() => false,
            Self::CloseCurly() => false,
            Self::CloseAngle() => false,
            Self::CloseSquare() => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    String(String),
    Char(String),
    Number(String),
}

impl Literal {
    pub fn concat(&mut self, c: char) -> Option<Token> {
        match self {
            Self::Number(s) => s.push(c),
            _ => todo!(),
        }
        None
    }

    pub fn parse(start: char, stream: &mut impl Iterator<Item = char>) -> Option<Self> {
        if matches!(start, '"' | '\'') {
            let mut ends = false;
            let mut s = String::new();
            while let Some(c) = stream.next() {
                if c == start {
                    ends = true;
                    break;
                }
                match c {
                    '\\' => {
                        s.push(c);
                        s.push(stream.next().unwrap_or('\0'));
                    }
                    _ => s.push(c),
                }
            }
            if !ends {
                panic!("Stream ended before end of literal");
            }
            Some(match start {
                '"' => Self::String(s),
                '\'' => Self::Char(s),
                _ => unreachable!(),
            })
        } else {
            None
        }
    }

    pub fn to_asm(&self) -> String {
        match self {
            Self::String(s) => format!("_str_{:X}", Sha1::default().digest(s.as_bytes()).finish()),
            Self::Char(c) => format!("'{}'", c),
            Self::Number(n) => n.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ident {
    /// Indicates an unknown identifier
    Unknown(String),
    Fn(),
    Let(),
    Const(),
    Mut(),
    Class(),
    If(),
    Else(),
    While(),
    For(),
}

impl Ident {
    pub fn is_char(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
    }

    pub fn concat(&mut self, c: char) -> Option<Token> {
        match self {
            Self::Unknown(s) => s.push(c),
            _ => panic!("Only unknown should be concatenated"),
        }
        None
    }

    pub fn finalize(self) -> Self {
        if let Self::Unknown(s) = self {
            match s.as_str() {
                "fn" => Self::Fn(),
                "let" => Self::Let(),
                "const" => Self::Const(),
                "mut" => Self::Mut(),
                "class" => Self::Class(),
                "if" => Self::If(),
                "else" => Self::Else(),
                "while" => Self::While(),
                "for" => Self::For(),
                _ => Self::Unknown(s),
            }
        } else {
            panic!("Only unknown should be finalized")
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Unknown(s) => &s,
            _ => "*INVALID*",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// Identifier
    Ident(Ident),
    Op(Operation),
    Literal(Literal),
    Group(Group),
    /// Whitespace, line ends, etc
    Noop(),
}

impl Token {
    pub fn parse(s: &str) -> impl Iterator<Item = Self> {
        let mut tokens = vec![];
        let mut stream = s.chars();
        while let Some(c) = stream.next() {
            if Ident::is_char(c) {
                tokens
                    .last_mut()
                    .unwrap_or(&mut Self::Noop())
                    .concat(c)
                    .map_or((), |new| tokens.push(new));
            } else if c.is_whitespace() {
                tokens.push(Self::Noop());
            } else if let Some(op) = Operation::from_char(c) {
                tokens.push(Token::Op(op));
            } else if let Some(group) = Group::from_char(c) {
                tokens.push(Token::Group(group));
            } else if let Some(lit) = Literal::parse(c, &mut stream) {
                tokens.push(Token::Literal(lit));
            } else {
                todo!("Unexpected token");
            }
        }
        tokens.into_iter().filter(|c| !matches!(c, Self::Noop()))
    }

    pub fn concat(&mut self, c: char) -> Option<Self> {
        match self {
            Self::Op(_) | Self::Noop() | Self::Group(_) => Some(Self::starting_char(c)),
            Self::Ident(ident) => ident.concat(c),
            Self::Literal(lit) => lit.concat(c),
            _ => todo!("Concat with unimplemented token"),
        }
    }

    pub fn starting_char(c: char) -> Self {
        match c {
            '0'..='9' => Self::Literal(Literal::Number(format!("{}", c))),
            'a'..='z' | 'A'..='Z' | '_' => Self::Ident(Ident::Unknown(format!("{}", c))),
            _ => todo!("Unknown starting character"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
    /// Identifier
    Ident(Ident),
    Op(Operation),
    Literal(Literal),
    Group(Group, Vec<TokenTree>),
}

impl TokenTree {
    pub fn from_iter(mut stream: impl Iterator<Item = Token>) -> Vec<Self> {
        Self::from_iter_part(&mut stream, None)
    }

    pub fn from_iter_part(
        stream: &mut impl Iterator<Item = Token>,
        mut close: Option<Group>,
    ) -> Vec<Self> {
        let mut v = vec![];
        while let Some(token) = stream.next() {
            match token {
                Token::Ident(ident) => v.push(Self::Ident(ident.finalize())),
                Token::Op(op) => v.push(Self::Op(op)),
                Token::Literal(lit) => v.push(Self::Literal(lit)),
                Token::Noop() => unreachable!(),
                Token::Group(g) => {
                    if close.as_ref().map_or(false, |f| f == &g) {
                        close = None;
                        break;
                    } else if g.is_open() {
                        v.push(Self::Group(
                            g,
                            Self::from_iter_part(stream, Some(g.close())),
                        ))
                    } else {
                        panic!("Invalid group")
                    }
                }
            }
        }
        if let Some(g) = close {
            panic!("Unmatched group: {:?}", g);
        }
        v
    }
}
