use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operation {
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
    fn from_char(c: char) -> Option<Self> {
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

    fn from_pair(a: &Self, b: &Self) -> Option<Self> {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Group {
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
    fn from_char(c: char) -> Option<Self> {
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

    fn close(&self) -> Self {
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

    fn is_open(&self) -> bool {
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
enum Literal {
    String(String),
    Char(String),
    Number(String),
}

impl Literal {
    fn concat(&mut self, c: char) -> Option<Token> {
        match self {
            Self::Number(s) => s.push(c),
            _ => todo!(),
        }
        None
    }

    fn parse(start: char, stream: &mut impl Iterator<Item = char>) -> Option<Self> {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ident {
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
    fn is_char(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
    }

    fn concat(&mut self, c: char) -> Option<Token> {
        match self {
            Self::Unknown(s) => s.push(c),
            _ => panic!("Only unknown should be concatenated"),
        }
        None
    }

    fn finalize(self) -> Self {
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
enum Token {
    /// Identifier
    Ident(Ident),
    Op(Operation),
    Literal(Literal),
    Group(Group),
    /// Whitespace, line ends, etc
    Noop(),
}

impl Token {
    fn parse(s: &str) -> impl Iterator<Item = Self> {
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

    fn concat(&mut self, c: char) -> Option<Self> {
        match self {
            Self::Op(_) | Self::Noop() | Self::Group(_) => Some(Self::starting_char(c)),
            Self::Ident(ident) => ident.concat(c),
            Self::Literal(lit) => lit.concat(c),
            _ => todo!("Concat with unimplemented token"),
        }
    }

    fn starting_char(c: char) -> Self {
        match c {
            '0'..='9' => Self::Literal(Literal::Number(format!("{}", c))),
            'a'..='z' | 'A'..='Z' | '_' => Self::Ident(Ident::Unknown(format!("{}", c))),
            _ => todo!("Unknown starting character"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenTree {
    /// Identifier
    Ident(Ident),
    Op(Operation),
    Literal(Literal),
    Group(Group, Vec<TokenTree>),
}

impl TokenTree {
    fn from_iter(mut stream: impl Iterator<Item = Token>) -> Vec<Self> {
        Self::from_iter_part(&mut stream, None)
    }

    fn from_iter_part(
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Params(Vec<Expression>);

impl Params {
    fn from_tt(stream: Vec<TokenTree>) -> Self {
        let mut s = vec![];
        let mut stream = stream.into_iter();
        while stream.len() > 0 {
            s.push(Expression::from_tt(
                None,
                &mut stream
                    .by_ref()
                    .take_while(|t| !matches!(t, &TokenTree::Op(Operation::Comma()))),
            ));
        }
        Self(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expression {
    FnCall {
        name: Box<Expression>,
        params: Params,
    },
    Binary {
        op: Operation,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        op: Operation,
        rhs: Box<Expression>,
    },
    Ident(Ident),
    Literal(Literal),
}

enum Either<T, V> {
    L(T),
    R(V),
}

impl<T: Debug, V: Debug> Debug for Either<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::L(v) => {
                write!(f, "l:")?;
                v.fmt(f)
            }
            Self::R(v) => {
                write!(f, "r:")?;
                v.fmt(f)
            }
        }
    }
}

fn replace_pair<T>(v: &mut Vec<T>, f: impl Fn(&T, &T) -> Option<T>) {
    let mut i = 0;
    while i + 1 < v.len() {
        if let Some(n) = f(&v[i], &v[i + 1]) {
            v.remove(i);
            v.push(n);
            v.swap_remove(i);
        } else {
            i += 1;
        }
    }
}

fn replace_op(v: &mut Vec<Either<TokenTree, Expression>>, f: impl Fn(&Operation) -> bool) {
    let mut i = 0;
    while i + 2 < v.len() {
        if let Either::L(TokenTree::Op(o)) = v[i + 1] {
            if f(&o) {
                let lhs = v.remove(i);
                let rhs = v.remove(i + 1);
                v.push(Either::R(Expression::Binary {
                    op: o,
                    lhs: Box::new(Expression::from_either(lhs)),
                    rhs: Box::new(Expression::from_either(rhs)),
                }));
                v.swap_remove(i);
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }
}

fn replace_unary(v: &mut Vec<Either<TokenTree, Expression>>, f: impl Fn(&Operation) -> bool) {
    let mut i = 0;
    while i + 1 < v.len() {
        if let Either::L(TokenTree::Op(o)) = v[i] {
            if f(&o) {
                let rhs = v.remove(i + 1);
                v.push(Either::R(Expression::Unary {
                    op: o,
                    rhs: Box::new(Expression::from_either(rhs)),
                }));
                v.swap_remove(i);
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }
}

impl Expression {
    fn from_either(e: Either<TokenTree, Self>) -> Self {
        match e {
            Either::L(t) => Self::from_tt(Some(t), &mut std::iter::empty()),
            Either::R(e) => e,
        }
    }

    fn from_tt(initial: Option<TokenTree>, stream: &mut impl Iterator<Item = TokenTree>) -> Self {
        let mut stream: Vec<Either<TokenTree, Expression>> = stream
            .take_while(|tt| !matches!(tt, TokenTree::Op(Operation::LineEnd())))
            .map(|tt| match tt {
                TokenTree::Ident(ident) => Either::R(Expression::Ident(ident)),
                TokenTree::Literal(lit) => Either::R(Expression::Literal(lit)),
                tt => Either::L(tt),
            })
            .collect();
        if let Some(initial) = initial {
            stream.insert(
                0,
                match initial {
                    TokenTree::Ident(ident) => Either::R(Expression::Ident(ident)),
                    TokenTree::Literal(lit) => Either::R(Expression::Literal(lit)),
                    tt => Either::L(tt),
                },
            );
        }
        // Replace pairs of operators
        replace_pair(&mut stream, |a, b| {
            if let Either::L(TokenTree::Op(a)) = a {
                if let Either::L(TokenTree::Op(b)) = b {
                    Operation::from_pair(a, b).map(|o| Either::L(TokenTree::Op(o)))
                } else {
                    None
                }
            } else {
                None
            }
        });
        replace_op(&mut stream, |op| match op {
            Operation::Dot() => true,
            _ => false,
        });
        // scan for function calls
        replace_pair(&mut stream, |a, b| {
            if let Either::L(TokenTree::Group(Group::OpenParen(), params)) = b {
                if let Either::L(TokenTree::Op(_)) = &a {
                    None
                } else if let Either::R(name) = a {
                    Some(Either::R(Expression::FnCall {
                        name: Box::new(name.clone()),
                        params: Params::from_tt(params.clone()),
                    }))
                } else {
                    None
                }
            } else {
                None
            }
        });
        replace_op(&mut stream, |op| match op {
            Operation::Mul() | Operation::Div() | Operation::Mod() => true,
            _ => false,
        });
        replace_op(&mut stream, |op| match op {
            Operation::Add() | Operation::Sub() => true,
            _ => false,
        });
        replace_unary(&mut stream, |op| match op {
            Operation::Sub() => true,
            _ => false,
        });
        replace_op(&mut stream, |op| match op {
            Operation::Assign() => true,
            _ => false,
        });
        if stream.len() > 1 {
            panic!("Not fully simplified")
        } else if let Some(Either::R(expr)) = stream.into_iter().nth(0) {
            expr
        } else {
            panic!("No Expr")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Statement {
    FunctionDef {
        name: Ident,
        params: Vec<(Ident, Ident)>,
        body: Vec<Statement>,
    },
    ConstDef {
        name: Ident,
        expr: Expression,
    },
    VarDef {
        name: Ident,
        expr: Expression,
    },
    Expression(Expression),
}

impl Statement {
    fn from_token_tree(tt: Vec<TokenTree>) -> Vec<Self> {
        let mut s = vec![];
        let mut stream = tt.into_iter();
        while let Some(tt) = stream.next() {
            if let TokenTree::Ident(i) = tt {
                match i {
                    Ident::Fn() => {
                        let name = if let Some(TokenTree::Ident(name)) = stream.next() {
                            name
                        } else {
                            panic!("fn must be followed by a name")
                        };
                        let params = if let Some(TokenTree::Group(Group::OpenParen(), params)) =
                            stream.next()
                        {
                            let mut p = vec![];
                            let mut stream = params.into_iter();
                            while stream.len() > 0 {
                                let mut t = stream.by_ref().take_while(|t| {
                                    !matches!(t, &TokenTree::Op(Operation::Comma()))
                                });
                                let name = if let Some(TokenTree::Ident(i)) = t.next() {
                                    i
                                } else {
                                    panic!("Parameter names must be idents")
                                };
                                if let Some(TokenTree::Op(Operation::Colon())) = t.next() {
                                } else {
                                    panic!("Parameter must have type")
                                };
                                let ty = if let Some(TokenTree::Ident(i)) = t.next() {
                                    i
                                } else {
                                    panic!("Parameter types must be idents")
                                };
                                assert!(t.next().is_none(), "type must be follwed by comma");
                                p.push((name, ty));
                            }
                            p
                        } else {
                            panic!("fn name must be followed by params")
                        };
                        let body = if let Some(TokenTree::Group(Group::OpenCurly(), body)) =
                            stream.next()
                        {
                            body
                        } else {
                            panic!("fn params must be followed by body")
                        };
                        s.push(Self::FunctionDef {
                            name,
                            params,
                            body: Self::from_token_tree(body),
                        });
                    }
                    Ident::Const() => {
                        let name = if let Some(TokenTree::Ident(name)) = stream.next() {
                            name
                        } else {
                            panic!("let must be followed by a name")
                        };
                        if let Some(TokenTree::Op(Operation::Assign())) = stream.next() {
                            // Do nothing with assign
                        } else {
                            panic!("let name must be followed by assignment")
                        }
                        s.push(Self::ConstDef {
                            name,
                            expr: Expression::from_tt(None, &mut stream),
                        });
                    }
                    Ident::Let() => {
                        let name = if let Some(TokenTree::Ident(name)) = stream.next() {
                            name
                        } else {
                            panic!("let must be followed by a name")
                        };
                        if let Some(TokenTree::Op(Operation::Assign())) = stream.next() {
                            // Do nothing with assign
                        } else {
                            panic!("let name must be followed by assignment")
                        }
                        s.push(Self::VarDef {
                            name,
                            expr: Expression::from_tt(None, &mut stream),
                        });
                    }
                    ident @ Ident::Unknown(_) => {
                        s.push(Self::Expression(Expression::from_tt(
                            Some(TokenTree::Ident(ident)),
                            &mut stream,
                        )));
                    }
                    _ => todo!(),
                }
            } else {
                panic!("Only idents valid");
            }
        }
        s
    }
}
const EXAMPLE_SOURCE: &str = "
    fn main() {
        let b = -1;
        let a = b.abs() + 12;
        printf(\"%n\", a);
    }
";

fn main() {
    let tokens = Statement::from_token_tree(TokenTree::from_iter(Token::parse(EXAMPLE_SOURCE)));
    println!("{:?}", tokens);
}
