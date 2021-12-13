use std::{borrow::Cow, io::BufRead};

use crate::token::{Ident, Punct, Span, Token, TokenStream};

//
// types.rs
// Copyright (C) 2021 matthew <matthew@matthew-ubuntu>
// Distributed under terms of the MIT license.
//

#[derive(Debug, Clone)]
pub enum Type {
    /// primitive number
    Primitive {
        floating: bool,
        signed: bool,
        size: usize,
        mutable: bool,
        span: Span,
    },
    /// 4 byte utf-8 character
    Char { mutable: bool, span: Span },
    /// a boolean
    Bool { mutable: bool, span: Span },
    /// An unamed tuple type
    Tuple { types: Vec<Type>, span: Span },
    /// A named type
    Named {
        name: Ident,
        mutable: bool,
        span: Span,
    },
    /// A refernce to an inner type
    Reference { ty: Box<Type>, span: Span },
    /// A pointer to an inner type
    #[allow(unused)]
    Pointer { ty: Box<Type>, span: Span },
}

impl Type {
    pub fn parse<R: BufRead>(stream: &mut TokenStream<R>) -> std::io::Result<Self> {
        match stream.parse()? {
            Token::Ident(Ident::User { val, span }) => match &val[..] {
                "usize" => Ok(Self::Primitive {
                    floating: false,
                    signed: false,
                    size: 0,
                    mutable: false,
                    span,
                }),
                "u64" => Ok(Self::Primitive {
                    floating: false,
                    signed: false,
                    size: 8,
                    mutable: false,
                    span,
                }),
                "u32" => Ok(Self::Primitive {
                    floating: false,
                    signed: false,
                    size: 4,
                    mutable: false,
                    span,
                }),
                "u16" => Ok(Self::Primitive {
                    floating: false,
                    signed: false,
                    size: 2,
                    mutable: false,
                    span,
                }),
                "u8" => Ok(Self::Primitive {
                    floating: false,
                    signed: false,
                    size: 1,
                    mutable: false,
                    span,
                }),
                "isize" => Ok(Self::Primitive {
                    floating: false,
                    signed: true,
                    size: 0,
                    mutable: false,
                    span,
                }),
                "i64" => Ok(Self::Primitive {
                    floating: false,
                    signed: true,
                    size: 8,
                    mutable: false,
                    span,
                }),
                "i32" => Ok(Self::Primitive {
                    floating: false,
                    signed: true,
                    size: 4,
                    mutable: false,
                    span,
                }),
                "i16" => Ok(Self::Primitive {
                    floating: false,
                    signed: true,
                    size: 2,
                    mutable: false,
                    span,
                }),
                "i8" => Ok(Self::Primitive {
                    floating: false,
                    signed: true,
                    size: 1,
                    mutable: false,
                    span,
                }),
                "f64" => Ok(Self::Primitive {
                    floating: true,
                    signed: true,
                    size: 8,
                    mutable: false,
                    span,
                }),
                "f32" => Ok(Self::Primitive {
                    floating: true,
                    signed: true,
                    size: 4,
                    mutable: false,
                    span,
                }),
                "char" => Ok(Self::Char {
                    mutable: false,
                    span,
                }),
                "bool" => Ok(Self::Bool {
                    mutable: false,
                    span,
                }),
                _ => Ok(Self::Named {
                    name: Ident::User { val, span },
                    mutable: false,
                    span,
                }),
            },
            Token::Punct(Punct { ch: '&', span }) => {
                let ty = Box::new(Self::parse(stream)?);
                Ok(Self::Reference {
                    span: span.union(&ty.span()),
                    ty,
                })
            }
            Token::Punct(Punct { ch: '*', .. }) => unimplemented!("Pointers are unimplemented"),
            Token::Ident(Ident::Mut(s)) => Ok(Self::parse(stream)?.as_mutable(s)),
            _ => panic!("Not a valid type"),
        }
    }

    fn mutable(&mut self, new_span: Span) {
        match self {
            Self::Primitive { mutable, span, .. }
            | Self::Char { mutable, span, .. }
            | Self::Bool { mutable, span, .. }
            | Self::Named { mutable, span, .. } => {
                *span = span.union(&new_span);
                *mutable = true;
            }
            Self::Reference { ty, span, .. } | Self::Pointer { ty, span, .. } => ty.mutable(*span),
            Self::Tuple { types, span, .. } => types.iter_mut().for_each(|t| t.mutable(*span)),
        }
    }

    fn as_mutable(mut self, span: Span) -> Self {
        self.mutable(span);
        self
    }

    pub fn empty() -> Self {
        Self::Tuple {
            types: vec![],
            span: Span::default(),
        }
    }

    pub fn boolean() -> Self {
        Self::Bool {
            mutable: false,
            span: Span::default(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Tuple { span, .. }
            | Self::Reference { span, .. }
            | Self::Pointer { span, .. }
            | Self::Primitive { span, .. }
            | Self::Char { span, .. }
            | Self::Bool { span, .. }
            | Self::Named { span, .. } => *span,
        }
    }

    pub fn width(&self) -> usize {
        match self {
            Type::Primitive {
                floating: _,
                signed: _,
                size,
                mutable: _,
                span: _,
            } => {
                if *size > 0 {
                    *size
                } else {
                    8
                }
            }
            Type::Char {
                mutable: _,
                span: _,
            } => 4,
            Type::Bool {
                mutable: _,
                span: _,
            } => 1,
            Type::Tuple { types, span: _ } => types.iter().map(|t| t.width()).sum(),
            Type::Named {
                name: _,
                mutable: _,
                span: _,
            } => todo!(),
            Type::Reference { ty: _, span: _ } => 8,
            Type::Pointer { ty: _, span: _ } => 8,
        }
    }

    pub fn as_str(&self) -> Cow<str> {
        match self {
            Type::Primitive {
                floating,
                signed,
                size,
                mutable: _,
                span: _,
            } => parts_to_str(*floating, *signed, *size),
            Type::Char {
                mutable: _,
                span: _,
            } => Cow::Borrowed("char"),
            Type::Bool {
                mutable: _,
                span: _,
            } => Cow::Borrowed("bool"),
            Type::Tuple { types, span: _ } => {
                let mut ret = String::from("T");
                for t in types {
                    ret += "_";
                    ret += &t.as_str();
                }
                ret += "_T";
                Cow::Owned(ret)
            }
            Type::Named {
                name,
                mutable: _,
                span: _,
            } => Cow::Owned(format!("N_{}", name.as_str())),
            Type::Reference { ty, span: _ } => Cow::Owned(format!("R_{}", ty.as_str())),
            Type::Pointer { ty, span: _ } => Cow::Owned(format!("P_{}", ty.as_str())),
        }
    }
}

fn parts_to_str(floating: bool, signed: bool, size: usize) -> Cow<'static, str> {
    if floating {
        match size {
            64 => Cow::Borrowed("f64"),
            32 => Cow::Borrowed("f32"),
            _ => panic!("Invalid type"),
        }
    } else if signed {
        match size {
            64 => Cow::Borrowed("i64"),
            32 => Cow::Borrowed("i32"),
            16 => Cow::Borrowed("i16"),
            8 => Cow::Borrowed("i8"),
            0 => Cow::Borrowed("isize"),
            _ => panic!("Invalid type"),
        }
    } else {
        match size {
            64 => Cow::Borrowed("u64"),
            32 => Cow::Borrowed("u32"),
            16 => Cow::Borrowed("u16"),
            8 => Cow::Borrowed("u8"),
            0 => Cow::Borrowed("usize"),
            _ => panic!("Invalid type"),
        }
    }
}
