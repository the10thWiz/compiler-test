//
// scope.rs
// Copyright (C) 2021 matthew <matthew@matthew-ubuntu>
// Distributed under terms of the MIT license.
//

use std::collections::LinkedList;

use crate::{
    asm::{BinaryOp, InnerAsmStream, Label, Location, UnaryOp},
    expression::Expression,
    statement::{Block, FunctionSignature, Statement},
    token::Ident,
    types::Type,
};

#[derive(Debug, Clone)]
pub struct Variable {
    name: Ident,
    location: Location,
}

#[derive(Debug, Clone, Default)]
pub struct LocalScope {
    pub functions: Vec<(FunctionSignature, Location)>,
    pub vars: Vec<Variable>,
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
                    let location = Location::Label {
                        name: sig.label(),
                        ty: sig.ty(),
                    };
                    self.functions.push((sig.clone(), location.clone()));
                    functions.push((sig, body, location));
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

    pub fn lookup_fn<'a>(&'a self, name: &Ident) -> Option<(&'a FunctionSignature, &'a Location)> {
        for (f, location) in self.functions.iter() {
            if f.name().as_str() == name.as_str() {
                return Some((f, location));
            }
        }
        None
    }
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub global: LocalScope,
    pub local: LinkedList<LocalScope>,
    pub asm: Vec<InnerAsmStream>,
    pub functions: Vec<(FunctionSignature, Block, Location)>,
    pub cur: InnerAsmStream,
    pub loop_head: Option<Label>,
    pub loop_tail: Option<Label>,
}

impl Scope {
    pub fn lookup<'a>(&'a self, name: &Ident) -> Option<&'a Location> {
        self.local
            .iter()
            .find_map(|s| s.lookup(name))
            .or_else(|| self.global.lookup(name))
    }

    pub fn lookup_fn<'a>(&'a self, name: &Ident) -> Option<(&'a FunctionSignature, &'a Location)> {
        self.local
            .iter()
            .find_map(|s| s.lookup_fn(name))
            .or_else(|| self.global.lookup_fn(name))
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
        let (sig, block, location) = self.functions.pop().unwrap();
        let mut scope = LocalScope::default();
        for (name, ty) in sig.params() {
            let location = self.cur.param(&ty);
            scope.add_local(Variable {
                name: name.clone(),
                location,
            });
        }
        match location {
            Location::Label { name, .. } => self.cur.exported_label(name),
            Location::Local { id: _, .. } => todo!(),
            Location::Param { num: _, .. } => todo!(),
        }
        self.parse_block(scope.into(), block);
        self.asm.push(std::mem::take(&mut self.cur))
    }

    pub fn parse_block(&mut self, scope: Option<LocalScope>, mut block: Block) {
        let mut scope = scope.unwrap_or_default();
        scope.add_signature(&mut block.statements, &mut self.functions);

        self.enter_scope(scope);
        for statement in block.statements {
            match statement {
                Statement::FunctionDef {
                    sig: _,
                    body: _,
                    span: _,
                } => todo!("This should never happen"),
                Statement::Const {
                    name: _,
                    ty: _,
                    value: _,
                    span: _s,
                } => todo!("Constants"),
                Statement::Let {
                    name,
                    ty,
                    value,
                    span: _s,
                } => {
                    //let mut location = self.cur.alloc(&Type::empty(), None);
                    let location = self.parse_expresion(value, ty);
                    self.current().add_local(Variable { name, location });
                }
                Statement::While {
                    condition: _,
                    body: _,
                    span: _s,
                } => todo!("While loops"),
                Statement::For {
                    vars: _,
                    iter: _,
                    body: _,
                    span: _s,
                } => todo!("For Loops"),
                Statement::Return { expr, span: _s } => {
                    let loc = self.parse_expresion(expr, None);
                    self.cur.return_fn(loc);
                }
                Statement::Break { expr: _, span: _s } => {
                    self.cur
                        .jump(self.loop_tail.clone().expect("Not in a loop"));
                }
                Statement::Continue { expr: _, span: _s } => {
                    self.cur
                        .jump(self.loop_head.clone().expect("Not in a loop"));
                }
                Statement::Block(block) => self.parse_block(None, block),
                Statement::If(if_chain) => {
                    assert!(if_chain.to_asm(self).is_none(), "No return type expected")
                }
                Statement::Implicit(expr) => {
                    let loc = self.parse_expresion(expr, None);
                    self.cur.return_fn(loc);
                }
                Statement::FnCall(fn_call, _s) => {
                    fn_call.to_asm(self);
                }
                Statement::Empty => (),
            }
        }
        self.leave_scope();
    }

    /// Assigns the result of expr to location
    pub fn parse_expresion(&mut self, expr: Expression, ty_hint: Option<Type>) -> Location {
        match expr {
            Expression::FnCall(fn_call, _s) => fn_call.to_asm(self),
            Expression::Value(lit) => {
                let loc = self
                    .cur
                    .local(ty_hint.as_ref().unwrap_or(&lit.default_type()), Some(lit));
                loc
            }
            Expression::Variable(v) => self.lookup(&v).cloned().expect("Variable not defined"),
            Expression::IfChain(_) => todo!(),
            Expression::Binary(lhs, op, rhs) => {
                let lhs = self.parse_expresion(*lhs, None);
                let rhs = self.parse_expresion(*rhs, None);
                let loc = self.cur.local(&lhs.ty(), None);
                self.cur.binary(BinaryOp::new(op), lhs, rhs, loc.clone());
                loc
            }
            Expression::Unary(op, rhs) => {
                let rhs = self.parse_expresion(*rhs, None);
                let loc = self.cur.local(&rhs.ty(), None);
                self.cur.unary(UnaryOp::new(op), rhs, loc.clone());
                loc
            }
            Expression::Tuple(_, _) => todo!(),
        }
    }

    pub fn from_statements(iter: impl Iterator<Item = Statement>) -> Self {
        let mut ret = Self::default();
        ret.parse_statements(iter);
        ret
    }

    pub fn parse_statements(&mut self, iter: impl Iterator<Item = Statement>) {
        let mut statements: Vec<_> = iter.collect();
        self.global
            .add_signature(&mut statements, &mut self.functions);
        // Note: we don't parse the global scope as a block, since we only allow signatures in the
        // global scope.
        //
        // After collecting signatures, we then expand them into the corresponding asm
        while !self.functions.is_empty() {
            self.parse_function();
        }
    }
}

// Namespaces?
