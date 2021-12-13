//
// scope.rs
// Copyright (C) 2021 matthew <matthew@matthew-ubuntu>
// Distributed under terms of the MIT license.
//

use std::{borrow::Cow, collections::LinkedList, sync::Weak};

use crate::{
    asm::{Asm, AsmParam},
    expression::Expression,
    statement::{Block, FunctionSignature, Statement},
    token::{Ident, Literal},
    types::Type,
};

#[derive(Debug, Clone)]
pub struct Variable {
    name: Ident,
    ty: Type,
    location: Location,
}

#[derive(Debug, Clone)]
pub enum Location {
    Stack { offset: usize, width: usize },
    Static { label: String, width: usize },
    Const { expr: Expression },
    Unknown,
}

impl AsmParam for Location {
    fn as_param(&self) -> Cow<str> {
        match self {
            Location::Stack { offset, width: _ } => Cow::Owned(format!("[sbp + {}]", offset)),
            Location::Static { label, width: _ } => label.as_param(),
            Location::Const { expr: _ } => todo!(),
            Location::Unknown => todo!(),
        }
    }
}
impl Location {
    fn set_width(&mut self, w: usize) {
        match self {
            Location::Stack { offset, width } => *width = w,
            Location::Static { label, width } => *width = w,
            Location::Const { .. } => (),
            Location::Unknown => (),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LocalScope {
    functions: Vec<(FunctionSignature, Location)>,
    vars: Vec<Variable>,
}

impl LocalScope {
    fn add_signature(
        &mut self,
        statements: &mut Vec<Statement>,
        functions: &mut Vec<(FunctionSignature, Block, Location)>,
    ) {
        let mut i = 0;
        while i < statements.len() {
            let tmp = statements.get_mut(i).unwrap();
            match std::mem::replace(tmp, Statement::Empty) {
                Statement::FunctionDef {
                    sig,
                    body,
                    span: _s,
                } => {
                    self.functions.push((sig.clone(), Location::Unknown));
                    functions.push((sig, body, Location::Unknown));
                    // Leaves an empty statement in the vector, which must be filtered later
                }
                a => {
                    let _ = std::mem::replace(tmp, a);
                    i += 1;
                }
            }
        }
    }

    fn add_local(&mut self, var: Variable) {
        if self
            .vars
            .iter()
            .any(|v| v.name.as_str() == var.name.as_str())
        {
            panic!("Variable already defined");
        }
        self.vars.push(var);
    }

    pub fn lookup<'a>(&'a self, name: &Ident) -> Option<&'a Location> {
        for var in self.vars.iter() {
            if var.name.as_str() == name.as_str() {
                return Some(&var.location);
            }
        }
        for (f, location) in self.functions.iter() {
            if f.name().as_str() == name.as_str() {
                return Some(location);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    global: LocalScope,
    local: LinkedList<LocalScope>,
    symbols: Vec<Symbol>,
    functions: Vec<(FunctionSignature, Block, Location)>,
    current: Symbol,
}

impl Scope {
    pub fn lookup<'a>(&'a self, name: &Ident) -> Option<&'a Location> {
        self.local
            .iter()
            .find_map(|s| s.lookup(name))
            .or_else(|| self.global.lookup(name))
    }

    pub fn enter_scope(&mut self, scope: LocalScope) {
        self.local.push_front(scope);
    }

    pub fn leave_scope(&mut self) {
        self.local.pop_front();
    }

    pub fn current(&mut self) -> &mut LocalScope {
        self.local.front_mut().unwrap()
    }

    fn parse_function(&mut self) {
        let (sig, mut block, location) = self.functions.pop().unwrap();

        let mut scope = LocalScope::default();
        scope.add_signature(&mut block.statements, &mut self.functions);
        for (name, ty) in sig.params() {
            scope.add_local(Variable {
                name: name.clone(),
                ty: ty.clone(),
                location: Location::Unknown,
            });
        }
        self.current = Symbol::Function {
            name: sig.name().as_str().to_owned(),
            sig,
            value: vec![],
        };

        self.enter_scope(scope);
        for statement in block.statements {
            match statement {
                Statement::FunctionDef { sig, body, span } => todo!("This should never happen"),
                Statement::Const {
                    name,
                    ty,
                    value,
                    span: _s,
                } => todo!(),
                Statement::Let {
                    name,
                    ty,
                    value,
                    span: _s,
                } => {
                    let mut location = Location::Stack {
                        offset: 0,
                        width: 0,
                    };
                    let ty = self.parse_expresion(&mut location, value, ty);
                    self.current().add_local(Variable { name, ty, location });
                }
                Statement::While {
                    condition,
                    body,
                    span: _s,
                } => todo!(),
                Statement::For {
                    vars,
                    iter,
                    body,
                    span: _s,
                } => todo!(),
                Statement::Return { expr, span: _s } => todo!(),
                Statement::Break { expr, span: _s } => todo!(),
                Statement::Continue { expr, span: _s } => todo!(),
                Statement::Block(block) => todo!(),
                Statement::If(if_chain) => (),
                Statement::Implicit(expr) => todo!(),
                Statement::FnCall(fncall, _s) => todo!(),
                Statement::Empty => (),
            }
        }
        self.leave_scope();
    }

    /// Assigns the result of expr to location
    fn parse_expresion(
        &mut self,
        location: &mut Location,
        expr: Expression,
        ty_hint: Option<Type>,
    ) -> Type {
        match expr {
            Expression::FnCall(_, _) => todo!(),
            Expression::Value(Literal::Number(n, _s)) => self.current.add(asm!(mov & location, n)),
            Expression::Value(lit) => todo!(),
            Expression::Variable(_) => todo!(),
            Expression::IfChain(_) => todo!(),
            Expression::Binary(_, _, _) => todo!(),
            Expression::Unary(_, _) => todo!(),
            Expression::Tuple(_, _) => todo!(),
        }
        //todo!("Eval: ?");
        let ty = if let Some(ty) = ty_hint {
            ty
        } else {
            Type::empty()
        };
        location.set_width(ty.width());
        ty
    }
}

impl Scope {
    pub fn from_statements(iter: impl Iterator<Item = Statement>) -> Self {
        let mut ret = Self::default();
        let mut statements: Vec<_> = iter.collect();
        ret.global
            .add_signature(&mut statements, &mut ret.functions);
        // Note: we don't parse the global scope as a block, since we only allow signatures in the
        // global scope.
        //
        // After collecting signatures, we then expand them into the corresponding symbols.
        //for (sig, block) in ret.functions {
        //ret.parse_block(block);
        //}
        while !ret.functions.is_empty() {
            ret.parse_function();
        }
        ret
    }
}

// Namespaces?

#[derive(Debug, Clone)]
pub enum Symbol {
    Empty,
    Function {
        sig: FunctionSignature,
        name: String,
        value: Vec<Asm>,
    },
    Const {
        name: String,
        value: Vec<Asm>,
    },
}

impl Symbol {
    pub fn add(&mut self, asm: Asm) {
        match self {
            Self::Empty => panic!("Unsupported operation"),
            Self::Function { value, .. } => value.push(asm),
            Self::Const { value, .. } => value.push(asm),
        }
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Self::Empty
    }
}
