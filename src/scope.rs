//
// scope.rs
// Copyright (C) 2021 matthew <matthew@matthew-ubuntu>
// Distributed under terms of the MIT license.
//

use crate::{statement::FunctionSignature, token::Ident, types::Type};

pub struct Variable {
    name: Ident,
    ty: Type,
}

pub struct LocalScope {
    functions: Vec<FunctionSignature>,
    consts: Vec<Variable>,
    vars: Vec<Variable>,
}

// Namespaces?
