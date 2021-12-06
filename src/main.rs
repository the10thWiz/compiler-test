use std::{
    fmt::{Debug, Write},
    hash::Hasher,
    ops::Deref,
    rc::Rc,
};

mod token;
use token::*;

use sha::{sha1::Sha1, utils::Digest};

macro_rules! asm {
    ($op:expr, $($arg:expr),*) => {
        Asm::Op {
            op: $op.to_string(),
            args: vec![$($arg.to_string()),*],
        }
    };
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
            function.body.push(asm!(
                "call",
                format!("{}_fn_{}", rhs.as_name(), lhs.as_name())
            ));
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
        let total: usize = params.0.iter().map(|p| 8).sum();
        // TODO: only add if non zero
        function.body.push(asm!("add", "rsp", format!("{}", total)));
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
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
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
                    Ident::While() => {
                        for token in stream {
                            println!("{:?}", token);
                        }
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
struct Type {
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
                        &format!("[{} rbp - {}]", size_to_word(size), offset),
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
