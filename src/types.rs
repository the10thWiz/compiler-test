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

    pub fn span(&self) -> Span {
        match self {
            Self::Tuple { span, .. }
            | Self::Reference { span, .. }
            | Self::Pointer { span, .. }
            | Self::Primitive { span, .. }
            | Self::Char { span, .. }
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
            Type::Char { mutable: _, span: _ } => 4,
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
                floating: true,
                signed: _,
                size: 32,
                mutable: _,
                span: _,
            } => Cow::Borrowed("f32"),
            Type::Primitive {
                floating: true,
                signed: _,
                size: 64,
                mutable: _,
                span: _,
            } => Cow::Borrowed("f64"),
            Type::Primitive {
                floating: false,
                signed: false,
                size: 64,
                mutable: _,
                span: _,
            } => Cow::Borrowed("u64"),
            Type::Primitive {
                floating: _,
                signed: _,
                size: _,
                mutable: _,
                span: _,
            } => todo!(),
            Type::Char { mutable: _, span: _ } => Cow::Borrowed("char"),
            Type::Tuple { types: _, span: _ } => todo!(),
            Type::Named {
                name: _,
                mutable: _,
                span: _,
            } => todo!(),
            Type::Reference { ty, span: _ } => Cow::Owned(format!("R_{}", ty.as_str())),
            Type::Pointer { ty, span: _ } => Cow::Owned(format!("P_{}", ty.as_str())),
        }
    }
}
