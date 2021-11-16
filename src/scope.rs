use std::collections::HashMap;

use crate::Type;

//
// scope.rs
// Copyright (C) 2021 matthew <matthew@WINDOWS-05HIC4F>
// Distributed under terms of the MIT license.
//

pub struct Value {
    raw: Vec<u8>,
    ty: Type,
}

pub enum Variable {
    Const { raw: Vec<u8>, ty: Type },
    Local { ty: Type },
    Function { args: Vec<Type> },
}

pub trait Scope {
    fn lookup_var(&self, s: &str) -> Option<&Variable>;
}

pub struct GlobalScope {
    vars: HashMap<String, Variable>,
}

impl Scope for GlobalScope {
    fn lookup_var(&self, s: &str) -> Option<&Variable> {
        self.vars.get(s)
    }
}

pub struct LocalScope<'a> {
    outer: &'a dyn Scope,
    vars: HashMap<String, Variable>,
}

impl<'a> Scope for LocalScope<'a> {
    fn lookup_var(&self, s: &str) -> Option<&Variable> {
        self.vars.get(s).or_else(|| self.outer.lookup_var(s))
    }
}

impl<'a> LocalScope<'a> {
    pub fn inner_scope<'b>(&'b self) -> LocalScope<'b> {
        LocalScope {
            outer: self,
            vars: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
