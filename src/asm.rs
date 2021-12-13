use std::borrow::Cow;

//
// asm.rs
// Copyright (C) 2021 matthew <matthew@matthew-ubuntu>
// Distributed under terms of the MIT license.
//

pub trait AsmParam {
    fn as_param(&self) -> Cow<str>;
}

impl AsmParam for String {
    fn as_param(&self) -> Cow<str> {
        Cow::Borrowed(self.as_str())
    }
}

impl AsmParam for usize {
    fn as_param(&self) -> Cow<str> {
        Cow::Owned(format!("{}", self))
    }
}

// Avoid issues with implementing AsmParam
impl<T: AsmParam> AsmParam for &mut T {
    fn as_param(&self) -> Cow<str> {
        T::as_param(&self)
    }
}

impl<T: AsmParam> AsmParam for &T {
    fn as_param(&self) -> Cow<str> {
        T::as_param(&self)
    }
}

#[derive(Debug, Clone)]
pub struct Asm(pub String);

macro_rules! asm {
    ($label:ident : $instruct:ident $($arg:expr),*) => {
        {
            let mut s = String::from(concat!(
                stringify!($label), ": \t",
                stringify!($instruct)
                ));
            $(
                s+= AsmParam::as_param(&tmp).as_ref();
                s+= ", "
            )*
            Asm(s)
        }
    };
    ($instruct:ident $($arg:expr),*) => {
        {
            use crate::asm::AsmParam;
            let mut s = String::from(concat!(
                stringify!($instruct)
                ));
            $(
                let tmp = $arg;
                s+= AsmParam::as_param(&tmp).as_ref();
                s+= ", ";
            )*
            Asm(s)
        }
    };
}

pub enum Registers {
    RAX,
    RBX,
}

impl AsmParam for Registers {
    fn as_param(&self) -> Cow<str> {
        match self {
            Self::RAX => Cow::Borrowed("rax"),
            Self::RBX => Cow::Borrowed("rbx"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        use Registers::*;
        let _asm = asm!(mov RAX, RBX);
    }
}
