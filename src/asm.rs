use crate::{expression::Operation, token::Literal, types::Type};

//
// asm.rs
// Copyright (C) 2021 matthew <matthew@matthew-ubuntu>
// Distributed under terms of the MIT license.
//

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Deref,
    Not,
    Ref,
}
impl UnaryOp {
    pub fn new(op: Operation) -> Self {
        match op {
            Operation::Not(_) => Self::Not,
            Operation::Ampersand(_) => Self::Ref,
            Operation::Multiplication(_) => Self::Deref,
            Operation::Subtraction(_) => Self::Neg,
            _ => panic!("Not a unary op"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    And,
    Or,
    Xor,
    // Comparison
    Equal,
    NotEqual,
    Greater,
    GreaterOrEq,
    Less,
    LessOrEq,
}

impl BinaryOp {
    pub fn new(op: Operation) -> Self {
        match op {
            Operation::Addition(_) => Self::Add,
            Operation::Subtraction(_) => Self::Sub,
            Operation::Multiplication(_) => Self::Mult,
            Operation::Division(_) => Self::Div,
            Operation::Modulo(_) => Self::Mod,
            Operation::Equality(_) => Self::Equal,
            Operation::InEquality(_) => Self::NotEqual,
            Operation::LessThan(_) => Self::Less,
            Operation::GreaterThan(_) => Self::Greater,
            Operation::LessEq(_) => Self::LessOrEq,
            Operation::GreaterEq(_) => Self::GreaterOrEq,
            Operation::And(_) | Operation::Ampersand(_) => Self::And,
            Operation::Or(_) | Operation::Pipe(_) => Self::Or,
            Operation::Xor(_) => Self::Xor,
            Operation::ShiftLeft(_) => todo!(),
            Operation::ShiftRight(_) => todo!(),

            _ => panic!("not a binary op"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Location {
    Label { name: String, ty: Type },
    Local { id: usize, ty: Type },
    Param { num: usize, ty: Type },
}

impl Location {
    pub fn set_type(&mut self, t: Type) {
        match self {
            Self::Label { name: _, ty } => *ty = t,
            Self::Local { id: _, ty } => *ty = t,
            Self::Param { num: _, ty } => *ty = t,
        }
    }

    pub fn ty(&self) -> &Type {
        match self {
            Self::Label { name: _, ty } => ty,
            Self::Local { id: _, ty } => ty,
            Self::Param { num: _, ty } => ty,
        }
    }
}

/// Inner Asm
///
/// Intermediate Asm language. This language has a concept of Local vs Param (rather than just a
/// stack). Globals (i.e. statics) must be converted into Labels, along with functions and similar.
/// There is a concept of function calls (with parameters).
///
/// In general there should be correspondence between this InnerAsm and actual assembly. Not
/// nessecarily 1 to 1, since a FnCall might map to something like:
/// ```
/// mov reg.., stack   ; save registers
/// mov param.., stack ; push parameters
/// call fn            ; Actual call
/// mov stack, reg..   ; restore registers
/// ```
///
/// The process for converting the internal AST into InnerAsm is not straight forward. First, each
/// local variable needs to be given it's own id on the stack. They should be allocated at the last
/// possible moment (i.e. right before the first assignment, preferably as a Some(src)). Stack
/// re-use should not be done, since multiple Locations with different ids can (and will) be mapped
/// to a single location on the stack. Binary and Unary ops must be ASM operations, i.e. overloaded
/// operators should be mapped to FnCalls in InnerAsm. The largest change from AST to Asm is the
/// lowering to branches, so if, while, for, etc must all be lowered to a set of branches.
///
/// To convert InnerAsm to Asm, first FnCalls must be lowered to just a jmp (represented as a
/// FnCall with no parameters). Second, Allocs need to be converted into stack modifications, which
/// can later be consolidated. Movs should be consolidated when possible (i.e. mov a -> b and mov b
/// -> c can be mov a -> c). Reordering should take place, when possible.
#[derive(Debug, Clone)]
pub enum InnerAsm {
    /// Preform binary operation
    BinaryOp {
        op: BinaryOp,
        lhs: Location,
        rhs: Location,
        res: Location,
    },
    /// Perform unary operation
    UnaryOp {
        op: UnaryOp,
        rhs: Location,
        res: Location,
    },
    /// Mov a value from one location to another
    Mov { src: Location, res: Location },
    /// Call function, passing params
    FnCall {
        function: Location,
        params: Vec<Location>,
        res: Location,
    },
    /// Conditionally branch
    ///
    /// Note: Only numeric labels supported
    Branch { cond: Option<Location>, to: usize },
    /// Define numeric label, and optionally export as a specific string
    Label { id: usize, exported: Option<String> },
    /// Allocate fixed size variable
    Alloc {
        src: Option<Literal>,
        location: Location,
    },
    /// Deallocate fixed size variable
    Delete { location: Location },
    /// Return from the function
    Return { val: Option<Location> },
}

#[derive(Debug, Default, Clone)]
pub struct InnerAsmStream {
    stream: Vec<InnerAsm>,
    stack: usize,
    param: usize,
    label: usize,
}

impl InnerAsmStream {
    /// Allocate local variable
    pub fn local(&mut self, ty: &Type, src: Option<Literal>) -> Location {
        let loc = Location::Local {
            id: self.stack,
            ty: ty.clone(),
        };
        self.stack += 1;
        self.stream.push(InnerAsm::Alloc {
            src,
            location: loc.clone(),
        });
        loc
    }

    pub fn delete(&mut self, location: Location) {
        self.stream.push(InnerAsm::Delete { location });
    }

    /// Define parameter parameter variable
    pub fn param(&mut self, ty: &Type) -> Location {
        let loc = Location::Param {
            num: self.param,
            ty: ty.clone(),
        };
        self.param += 1;
        loc
    }

    /// Move value from a to b
    pub fn mov(&mut self, from: Location, to: Location) {
        self.stream.push(InnerAsm::Mov { src: from, res: to })
    }

    pub fn local_label(&mut self) -> Label {
        let cur = self.label;
        self.label += 1;
        Label(cur)
    }

    pub fn label(&mut self, label: Label) {
        self.stream.push(InnerAsm::Label {
            id: label.0,
            exported: None,
        });
    }

    pub fn exported_label(&mut self, label: String) {
        self.stream.push(InnerAsm::Label {
            id: self.label,
            exported: Some(label),
        });
        self.label += 1;
    }

    pub fn branch(&mut self, cond: Location, label: Label) {
        self.stream.push(InnerAsm::Branch {
            cond: Some(cond),
            to: label.0,
        });
    }

    pub fn jump(&mut self, label: Label) {
        self.stream.push(InnerAsm::Branch {
            cond: None,
            to: label.0,
        });
    }

    pub fn unary(&mut self, op: UnaryOp, rhs: Location, res: Location) {
        self.stream.push(InnerAsm::UnaryOp { op, rhs, res });
    }

    pub fn binary(&mut self, op: BinaryOp, lhs: Location, rhs: Location, res: Location) {
        self.stream.push(InnerAsm::BinaryOp { op, rhs, lhs, res });
    }

    pub fn fn_call(&mut self, name: Location, params: Vec<Location>, res: Location) {
        self.stream.push(InnerAsm::FnCall {
            function: name,
            params,
            res,
        });
    }

    pub fn return_fn(&mut self, loc: Location) {
        self.stream.push(InnerAsm::Return { val: Some(loc) });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Label(usize);
