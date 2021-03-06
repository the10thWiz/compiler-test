use std::{
    fmt::{Debug, Write},
    hash::Hasher,
    ops::Deref,
    rc::Rc,
};

use sha::{sha1::Sha1, utils::Digest};

mod scope;
mod vars;

macro_rules! asm {
    ($op:expr, $($arg:expr),*) => {
        Asm::Op {
            op: $op.to_string(),
            args: vec![$($arg.to_string()),*],
        }
    };
}

mod scope;
mod vars;

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

    fn as_str(&self) -> &str {
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

    fn to_asm(&self) -> String {
        match self {
            Self::String(s) => format!("_str_{:X}", Sha1::default().digest(s.as_bytes()).finish()),
            Self::Char(c) => format!("'{}'", c),
            Self::Number(n) => n.clone(),
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
            let is_unary = if i > 0 {
                // if the previous value is a token, it's a unary operation
                matches!(v[i - 1], Either::L(_))
            } else {
                true
            };
            if is_unary && f(&o) {
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
        replace_unary(&mut stream, |op| match op {
            // Negate, ref, deref
            Operation::Sub() | Operation::And() | Operation::Mul() => true,
            _ => false,
        });
        replace_op(&mut stream, |op| match op {
            Operation::Mul() | Operation::Div() | Operation::Mod() => true,
            _ => false,
        });
        replace_op(&mut stream, |op| match op {
            Operation::Add() | Operation::Sub() => true,
            _ => false,
        });
        replace_op(&mut stream, |op| match op {
            Operation::Assign() => true,
            _ => false,
        });
        if stream.len() > 1 {
            println!("{:?}", stream);
            panic!("Not fully simplified")
        } else if let Some(Either::R(expr)) = stream.into_iter().nth(0) {
            expr
        } else {
            panic!("No Expr")
        }
    }

    fn to_asm(&self, function: &mut Function) {
        match self {
            Expression::FnCall { name, params } => Self::fncall_asm(function, name, params),
            Expression::Binary { op, lhs, rhs } => todo!(),
            Expression::Unary { op, rhs } => todo!(),
            Expression::Ident(_) => todo!(),
            Expression::Literal(lit) => panic!("Invalid"),
        }
    }

    fn to_asm_push(&self, function: &mut Function) {
        match self {
            Expression::FnCall { name, params } => {
                Self::fncall_asm(function, name, params);
                function.body.push(asm!("push", "rax"));
            }
            Expression::Binary { op, lhs, rhs } => {
                Self::fncall_asm_op(
                    function,
                    format!("{}_fn_Zu64_Zu64", op.as_str()),
                    &Params(vec![Expression::clone(lhs), Expression::clone(rhs)]),
                );
            }
            Expression::Unary { op, rhs } => {
                Self::fncall_asm_op(
                    function,
                    format!("{}_fn_Zu64", op.as_str()),
                    &Params(vec![Expression::clone(rhs)]),
                );
                function.body.push(asm!("push", "rax"));
            }
            Expression::Ident(i) => function
                .body
                .push(asm!("push", function.offset(i.as_str()))),
            Expression::Literal(lit) => {
                function.global(lit);
                function.body.push(asm!("push", lit.to_asm()))
            }
        }
    }

    fn to_asm_assign(&self, function: &mut Function, offset: impl AsRef<str>) {
        match self {
            Expression::FnCall { name, params } => {
                Self::fncall_asm(function, name, params);
                function.body.push(asm!("mov", offset.as_ref(), "rax"))
            }
            Expression::Binary { op, lhs, rhs } => {
                Self::fncall_asm_op(
                    function,
                    format!("{}_fn_Zu64_Zu64", op.as_str()),
                    &Params(vec![Expression::clone(lhs), Expression::clone(rhs)]),
                );
                function.body.push(asm!("mov", offset.as_ref(), "rax"))
            }
            Expression::Unary { op, rhs } => {
                Self::fncall_asm_op(
                    function,
                    format!("{}_fn_Zu64", op.as_str()),
                    &Params(vec![Expression::clone(rhs)]),
                );
                function.body.push(asm!("mov", offset.as_ref(), "rax"))
            }
            Expression::Ident(i) => {
                function
                    .body
                    .push(asm!("mov", offset.as_ref(), function.offset(i.as_str())))
            }
            Expression::Literal(lit) => {
                function.global(lit);
                function
                    .body
                    .push(asm!("mov", offset.as_ref(), lit.to_asm()))
            }
        }
        //function.body.push(asm!())
    }

    fn fncall_asm_op(function: &mut Function, name: impl AsRef<str>, params: &Params) {
        Self::fncall_asm(
            function,
            &Box::new(Expression::Ident(Ident::Unknown(name.as_ref().to_string()))),
            params,
        )
    }

    fn fncall_asm(function: &mut Function, name: &Box<Expression>, params: &Params) {
        //function.body.push()
        for param in params.0.iter().rev() {
            param.to_asm_push(function);
        }
        if let Expression::Binary {
            op: Operation::Dot(),
            lhs,
            rhs,
        } = name.deref()
        {
            lhs.to_asm_push(function);
            function
                .body
                .push(asm!("call", format!("{}_fn", rhs.as_name())));
            function.body.push(asm!("pop",));
        } else if let Expression::Binary {
            op: Operation::Namespace(),
            lhs: _lhs,
            rhs,
        } = name.deref()
        {
            function
                .body
                .push(asm!("call", format!("{}_fn", rhs.as_name())));
        } else {
            function.body.push(asm!("call", name.as_name()));
        }
        for _ in params.0.iter() {
            function.body.push(asm!("pop",));
        }
    }

    fn as_name(&self) -> String {
        match self {
            Self::Ident(i) => format!("{}", i.as_str()),
            Self::FnCall { name, params } => todo!(),
            Self::Binary { op, lhs, rhs } => {
                assert!(
                    matches!(op, Operation::Dot() | Operation::Namespace()),
                    "Only dot and namespace are valid"
                );
                format!("{}_{}_{}", lhs.as_name(), op.as_str(), rhs.as_name())
            }
            Self::Unary { op, rhs } => todo!(),
            Self::Literal(_) => todo!(),
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

#[derive(Debug)]
pub struct Type {
    name: String,
    /// Size in bytes
    size: usize,
}

impl Type {
    fn new(name: impl AsRef<str>, size: usize) -> Rc<Self> {
        Rc::new(Self {
            name: name.as_ref().to_string(),
            size,
        })
    }
}

#[derive(Debug)]
struct Variable {
    name: String,
    ty: Rc<Type>,
    address: Option<usize>,
}

#[derive(Debug)]
struct GlobalScope {
    types: Vec<Rc<Type>>,
    consts: Vec<Variable>,
    functions: Vec<Function>,
}

impl GlobalScope {
    fn new() -> Self {
        Self {
            consts: vec![],
            functions: vec![],
            types: vec![
                Type::new("usize", 8),
                Type::new("u64", 8),
                Type::new("u32", 4),
                Type::new("u16", 2),
                Type::new("u8", 1),
                Type::new("pointer", 8),
            ],
        }
    }

    fn new_type(&mut self, name: impl AsRef<str>, size: usize) -> Rc<Type> {
        let name = name.as_ref();
        for ty in self.types.iter() {
            if ty.name == name {
                assert_eq!(ty.size, size, "Redefinition with a different size");
                return ty.clone();
            }
        }
        self.types.push(Type::new(name, size));
        self.types.last().unwrap().clone()
    }
    fn get_type(&mut self, name: impl AsRef<str>) -> Rc<Type> {
        let name = name.as_ref();
        for ty in self.types.iter() {
            if ty.name == name {
                return ty.clone();
            }
        }
        panic!("Unknown type")
    }
}

#[derive(Debug)]
enum Asm {
    Op { op: String, args: Vec<String> },
    Label(String),
    Other(String),
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Label(name) => write!(f, "{}:", name),
            Self::Other(name) => write!(f, "{}", name),
            Self::Op { op, args } => {
                write!(f, "\t{} {}", op, args.get(0).unwrap_or(&"".to_string()))?;
                for arg in args.iter().skip(1) {
                    write!(f, ", {}", arg)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
struct Function {
    name: String,
    params: Vec<Variable>,
    scope: Vec<Variable>,
    body: Vec<Asm>,
    strings: Vec<String>,
}

fn size_to_word(size: usize) -> &'static str {
    match size {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
        _ => panic!("Unsupported size"),
    }
}

const FN_WRAPPER: bool = true;

impl Function {
    fn global(&mut self, lit: &Literal) {
        if let Literal::String(s) = lit {
            self.strings.push(s.clone());
        }
    }

    fn from_statement(
        global: &mut GlobalScope,
        ident: Ident,
        params: Vec<(Ident, Ident)>,
        body: Vec<Statement>,
    ) {
        let mut s = Self {
            name: ident.as_str().to_string(),
            params: vec![],
            scope: vec![],
            body: vec![],
            strings: vec![],
        };

        for (name, ty) in params {
            if s.params.iter().any(|v| v.name == name.as_str()) {
                panic!("Redefined parameter name");
            }
            s.params.push(Variable {
                name: name.as_str().to_string(),
                ty: global.get_type(ty.as_str()),
                address: None,
            });
        }

        let mut offset = 0;
        for param in s.params.iter_mut().rev() {
            offset += param.ty.size;
            param.address = Some(offset);
        }

        let mut offset = 0;
        for statement in body.iter() {
            match statement {
                Statement::FunctionDef { name, params, body } => todo!(),
                Statement::Expression(expr) => {
                    expr.to_asm_push(&mut s);
                }
                Statement::ConstDef { name, expr } | Statement::VarDef { name, expr } => {
                    if s.scope.iter().any(|v| v.name == name.as_str()) {
                        panic!("Redefined variable name");
                    }
                    s.scope.push(Variable {
                        name: name.as_str().to_string(),
                        ty: global.get_type("usize"),
                        address: Some(offset),
                    });
                    let size = s.scope.last().unwrap().ty.size;
                    expr.to_asm_assign(
                        &mut s,
                        &format!("[{}, rbp - {}]", size_to_word(size), offset),
                    );
                    offset += size;
                    // s.scope.last().unwrap()
                }
            }
        }

        if FN_WRAPPER {
            // Prelude
            s.body.insert(0, Asm::Other(format!("global {}", s.name)));
            s.body.insert(1, Asm::Label(s.name.clone())); // Function label
            s.body.insert(2, asm!("push", "rbp")); // Save base pointer
            s.body.insert(3, asm!("mov", "rbp", "rsp")); // Set new base pointer
            s.body.insert(4, asm!("sub", "rsp", format!("{}", offset))); // Allocate space for local vars
            s.body.insert(5, asm!("push", "rbx"));
            s.body.insert(6, asm!("push", "rdi"));
            s.body.insert(7, asm!("push", "rsi")); // Save callee saved registers

            // ending
            s.body.push(asm!("pop", "rsi")); // Restore callee saved registers
            s.body.push(asm!("pop", "rdi"));
            s.body.push(asm!("pop", "rbx"));
            s.body.push(asm!("mov", "rsp", "rbp")); // Deallocate local vars
            s.body.push(asm!("pop", "rbp")); // Restore base pointer
            s.body.push(asm!("ret",)); // Return
        }

        global.functions.push(s);
    }

    fn offset(&self, var: impl AsRef<str>) -> String {
        let var = var.as_ref();
        for local in self.scope.iter() {
            if local.name == var {
                return format!(
                    "[{} rbp - {}]",
                    size_to_word(local.ty.size),
                    local
                        .address
                        .expect("Local vars should always have offsets")
                );
            }
        }
        for local in self.params.iter() {
            if local.name == var {
                return format!(
                    "[{} rbp + {}]",
                    size_to_word(local.ty.size),
                    local
                        .address
                        .expect("Local vars should always have offsets")
                );
            }
        }
        panic!("Variabe not found")
    }

    fn as_signature(&self) -> FunctionSignature {
        FunctionSignature {
            name: self.name.clone(),
            params: self.params.iter().map(|v| v.ty.clone()).collect(),
        }
    }

    fn asm(&self) -> String {
        let mut ret = String::new();
        for asm in self.body.iter() {
            write!(ret, "{}\n", asm).unwrap();
        }
        ret
    }
}

struct FunctionSignature {
    name: String,
    params: Vec<Rc<Type>>,
}

const EXAMPLE_SOURCE: &str = "
    fn main(b: u64) {
        let b = -1;
        let a = b.abs() + 12;
        printf(\"%n\", a);
    }
";

fn main() {
    let tokens = Token::parse(EXAMPLE_SOURCE);
    let token_tree = TokenTree::from_iter(tokens);
    let statements = Statement::from_token_tree(token_tree);
    //println!("{:?}", statements);
    let mut global = GlobalScope::new();
    for s in statements {
        match s {
            Statement::FunctionDef { name, params, body } => {
                Function::from_statement(&mut global, name, params, body)
            }
            Statement::ConstDef { name, expr } => (),
            _ => todo!(),
        }
    }
    println!("section .rodata");
    for f in global.functions.iter() {
        for s in f.strings.iter() {
            println!(
                "_str_{:X}: db '{}'",
                Sha1::default().digest(s.as_bytes()).finish(),
                s
            );
        }
    }
    println!("section .text");
    for f in global.functions.iter() {
        println!("{}", f.asm());
    }
}
