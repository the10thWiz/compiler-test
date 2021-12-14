use std::{
    io::{BufRead, Result},
    ops::Range,
};

/// Note - always compares the same, to make implementing eq easier?
#[derive(Clone, Copy, Hash, Default)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    #[allow(unused)]
    pub fn as_range(&self) -> Range<usize> {
        Range {
            start: self.start,
            end: self.end,
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl std::cmp::PartialEq for Span {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl std::cmp::Eq for Span {}

macro_rules! keywords {
    ($a:ident) => {
        $a! {
            Let => "let",
            Const => "const",
            If => "if",
            Else => "else",
            While => "while",
            For => "for",
            In => "in",
            Fun => "fn",
            Return => "return",
            Yeild => "yeild",
            Break => "break",
            Continue => "continue",
            Async => "async",
            Await => "await",
            Struct => "struct",
            Enum => "enum",
            Use => "use",
            Pub => "pub",
            Mut => "mut",
            Unsafe => "unsafe"
        }
    };
}

macro_rules! define {
    ($($a:ident => $name:literal),*) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum Ident {
            $($a(Span),)*
            User { val: String, span: Span },
        }
    }
}

keywords!(define);

impl Ident {
    pub fn as_str(&self) -> &str {
        macro_rules! as_str {
            ($($a:ident => $name:literal),*) => {
                match self {
                    $(Self::$a(_) => $name,)*
                    Self::User { val, .. } => val,
                }
            };
        }
        keywords!(as_str)
    }

    fn len(&self) -> usize {
        self.as_str().len()
    }

    fn is_valid_start(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_valid(ch: char) -> bool {
        Self::is_valid_start(ch) || ch.is_digit(10)
    }

    fn parse(ch: char, mut iter: impl Iterator<Item = char>) -> Option<Self> {
        let mut ret = ch.to_string();
        macro_rules! from_str {
            ($($a:ident => $name:literal),*) => {
                match ret.as_str() {
                    $($name => Self::$a(Span::default()),)*
                    _ => Self::User { val: ret, span: Span::default() },
                }
            };
        }
        loop {
            let next = iter.next()?;
            if Self::is_valid(next) {
                ret.push(next);
            } else {
                break Some(keywords!(from_str));
            }
        }
    }

    pub fn is_keyword(&self) -> bool {
        macro_rules! is_keyword {
            ($($a:ident => $name:literal),*) => {
                match self {
                    $(Self::$a(_) => true,)*
                    Self::User { .. } => false,
                }
            };
        }
        keywords!(is_keyword)
    }

    pub fn set_span(&mut self, span: Span) {
        macro_rules! set_span {
            ($($a:ident => $name:literal),*) => {
                match self {
                    $(Self::$a(s) => *s = span,)*
                    Self::User { span: s, .. } => *s = span,
                }
            };
        }
        keywords!(set_span)
    }

    pub fn span(&self) -> Span {
        macro_rules! span {
            ($($a:ident => $name:literal),*) => {
                match self {
                    $(Self::$a(s) => *s,)*
                    Self::User { span: s, .. } => *s,
                }
            };
        }
        keywords!(span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punct {
    pub ch: char,
    pub span: Span,
}

impl Punct {
    fn is_valid(ch: char) -> bool {
        match ch {
            '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '(' | ')' | '-' | '+' | '=' | '['
            | ']' | '{' | '}' | '|' | '\\' | '/' | '<' | '>' | ',' | '.' | '?' | ';' | ':' => true,
            _ => false,
        }
    }

    fn len(&self) -> usize {
        self.ch.len_utf8()
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Number(String, Span),
    String(String, Span),
    Char(String, Span),
}

impl Literal {
    fn is_start(ch: char) -> bool {
        match ch {
            '\'' | '"' | '0'..='9' => true,
            _ => false,
        }
    }

    fn parse(ch: char, mut iter: impl Iterator<Item = char>) -> Option<Self> {
        match ch {
            '\'' => {
                let mut ch = iter.next()?.to_string();
                if ch == "'" {
                    return None;
                }
                if ch == "\\" {
                    ch.push(iter.next()?);
                }
                if iter.next()? == '\'' {
                    Some(Self::Char(ch, Span::default()))
                } else {
                    None
                }
            }
            '"' => {
                let mut ch = String::new();
                loop {
                    let next = iter.next()?;
                    if next == '\\' {
                        ch.push(next);
                        ch.push(iter.next()?);
                    } else if next == '"' {
                        break Some(Self::String(ch, Span::default()));
                    } else {
                        ch.push(next);
                    }
                }
            }
            '0'..='9' => {
                let mut ch = ch.to_string();
                let radix = if ch == "0" {
                    // Special case
                    let next = iter.next()?;
                    match next {
                        'x' | 'X' => {
                            ch.push(next);
                            16
                        }
                        'b' => {
                            ch.push(next);
                            2
                        }
                        'o' => {
                            ch.push(next);
                            8
                        }
                        c if c.is_digit(10) => {
                            ch.push(next);
                            10
                        }
                        _ => return Some(Self::Number(ch, Span::default())),
                    }
                } else {
                    10
                };
                let mut has_decimal = false;
                loop {
                    let next = iter.next()?;
                    if next.is_digit(radix) || next == '_' {
                        ch.push(next);
                    } else if next == '.' {
                        if has_decimal {
                            break None;
                        }
                        ch.push(next);
                        has_decimal = true;
                    } else if matches!(next, 'u' | 'i') {
                        todo!("Number Suffix")
                    } else {
                        break Some(Self::Number(ch, Span::default()));
                    }
                }
            }
            _ => None,
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::Number(s, _) => s.len(),
            Self::String(s, _) => s.len() + 2,
            Self::Char(s, _) => s.len() + 2,
        }
    }

    pub fn set_span(&mut self, span: Span) {
        match self {
            Self::Number(_, s) => *s = span,
            Self::String(_, s) => *s = span,
            Self::Char(_, s) => *s = span,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Number(_, s) => *s,
            Self::String(_, s) => *s,
            Self::Char(_, s) => *s,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    EOF,
}

impl Token {
    fn parse_from(mut iter: impl Iterator<Item = char>) -> Option<Token> {
        match iter.next() {
            Some(ch) if Punct::is_valid(ch) => Some(Token::Punct(Punct {
                ch,
                span: Span::default(),
            })),
            Some(ch) if Literal::is_start(ch) => Some(Token::Literal(Literal::parse(ch, iter)?)),
            Some(ch) if Ident::is_valid_start(ch) => Some(Token::Ident(Ident::parse(ch, iter)?)),
            _ => None,
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::Ident(i) => i.len(),
            Self::Punct(i) => i.len(),
            Self::Literal(i) => i.len(),
            Self::EOF => 0,
        }
    }

    pub fn set_span(&mut self, span: Span) {
        match self {
            Self::Ident(i) => i.set_span(span),
            Self::Punct(i) => i.set_span(span),
            Self::Literal(i) => i.set_span(span),
            Self::EOF => (),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.set_span(span);
        self
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Ident(i) => i.span(),
            Self::Punct(i) => i.span(),
            Self::Literal(i) => i.span(),
            Self::EOF => Span::default(),
        }
    }
}

pub struct TokenStream<R: BufRead> {
    source: R,
    reached_eof: bool,
    pos: usize,
    peek: Option<Token>,
}

impl<R: BufRead> TokenStream<R> {
    pub fn new(source: R) -> Self {
        Self {
            source,
            reached_eof: false,
            pos: 0,
            peek: None,
        }
    }

    pub fn parse_raw_token(&mut self) -> Result<Token> {
        if let Some(peek) = self.peek.take() {
            return Ok(peek);
        }
        if self.reached_eof {
            return Ok(Token::EOF);
        }
        let buffer = valid_portion(self.source.fill_buf()?);
        let trimmed = buffer.trim_start();
        let whitespace = buffer.len() - trimmed.len();
        self.pos += whitespace;
        let token = match Token::parse_from(trimmed.chars()) {
            Some(token) => {
                self.source.consume(whitespace + token.len());
                token
            }
            None => {
                let tmp = trimmed.to_owned();
                let buffer_len = buffer.len();
                self.source.consume(buffer_len);
                let buffer = valid_portion(self.source.fill_buf()?);
                if tmp.len() + buffer.len() == 0 {
                    self.reached_eof = true;
                    return Ok(Token::EOF);
                }
                Token::parse_from(tmp.chars().chain(buffer.chars())).expect("Token to large")
            }
        };
        let start = self.pos;
        let end = self.pos + token.len();
        self.pos = end;
        Ok(token.with_span(Span { start, end }))
    }

    pub fn parse(&mut self) -> Result<Token> {
        loop {
            let token = self.parse_raw_token()?;
            if matches!(token, Token::Punct(Punct { ch: '/', .. })) {
                let buffer = valid_portion(self.source.fill_buf()?);
                if buffer.starts_with('/') {
                    self.read_until("\n")?;
                } else if buffer.starts_with('*') {
                    self.read_until("*/")?;
                } else {
                    break Ok(token);
                }
            } else {
                break Ok(token);
            }
        }
    }

    fn read_until(&mut self, s: &str) -> Result<()> {
        // TODO: handle potential issues with readbuf not reading enough for multiline comments
        loop {
            let buffer = valid_portion(self.source.fill_buf()?);
            if let Some(idx) = buffer.find(s) {
                self.source.consume(idx + s.len());
                break Ok(());
            } else {
                let len = buffer.len();
                self.source.consume(len - s.len());
            }
        }
    }

    pub fn peek(&mut self) -> Result<&Token> {
        // This should be an if let, but the borrow checker can't handle it
        if self.peek.is_some() {
            Ok(self.peek.as_ref().unwrap())
        } else {
            match self.parse() {
                Ok(token) => {
                    self.peek = Some(token);
                    Ok(self.peek.as_ref().unwrap())
                }
                Err(e) => Err(e),
            }
        }
    }

    pub fn un_peek(&mut self, token: Token) {
        assert!(self.peek.is_none(), "Already has value in peek");
        self.peek = Some(token);
    }
}

impl<R: BufRead> Iterator for TokenStream<R> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if self.reached_eof {
            None
        } else {
            self.parse().ok()
        }
    }
}

/// Returns buffer as a utf-8 str, excluding up to the last 3 bytes if they are a partial UTF-8
/// sequence.
fn valid_portion(buffer: &[u8]) -> &str {
    match std::str::from_utf8(buffer) {
        Ok(s) => s,
        Err(e) => std::str::from_utf8(&buffer[..e.valid_up_to()]).unwrap(),
    }
}

impl<R: BufRead> std::fmt::Debug for TokenStream<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TokenStream {{ pos: {}, peek: {:?}}}",
            self.pos, self.peek
        )
    }
}
