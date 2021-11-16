//
// vars.rs
// Copyright (C) 2021 matthew <matthew@WINDOWS-05HIC4F>
// Distributed under terms of the MIT license.
//

use std::rc::Rc;

pub enum Location {
    Label(String),
    Pointer(Box<Variable>),
    Stack(usize),
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
pub struct LocalType {
    ty: Rc<Type>,
    reference: bool,
    mutable: bool,
}

pub enum Variable {
    Const {
        raw: Vec<u8>,
        ty: LocalType,
        addr: Location,
    },
    Local {
        ty: LocalType,
        addr: Location,
    },
    Function {
        args: Vec<LocalType>,
        addr: Location,
    },
}
