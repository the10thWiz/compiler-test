use std::io::{BufRead};

use crate::expression::{Expression, FnCall, IfChain};

use crate::token::{Ident, Punct, Span, Token, TokenStream};
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    name: Ident,
    params: Vec<(Ident, Type)>,
    ret_type: Type,
}

macro_rules! expect {
    ($stream:expr, $token:pat) => {
        match $stream.parse()? {
            token @ $token => token,
            _ => panic!("Unexpected token"),
        }
    };
    ($stream:expr, $token:pat if $ex:expr) => {{
        let token = $stream.parse()?;
        match &token {
            $token if $ex => (),
            _ => panic!("Unexpected token"),
        }
        token
    }};
    ($stream:expr, $token:pat => $n:ident) => {
        match $stream.parse()? {
            $token => $n,
            _ => panic!("Unexpected token"),
        }
    };
    ($stream:expr, $token:pat if $ex:expr => $n:ident) => {
        match $stream.parse()? {
            $token if $ex => $n,
            _ => panic!("Unexpected token"),
        }
    };
}

impl FunctionSignature {
    pub fn parse<R: BufRead>(stream: &mut TokenStream<R>) -> std::io::Result<(Self, Span)> {
        let name = expect!(
            stream,
            Token::Ident(i) if !i.is_keyword() => i
        );
        expect!(stream, Token::Punct(Punct { ch: '(', .. }));
        let mut params = vec![];
        loop {
            let name = match stream.parse()? {
                Token::Ident(i) if !i.is_keyword() => i,
                Token::Punct(Punct { ch: ')', .. }) => break,
                _ => panic!("Unexpected token"),
            };
            expect!(stream, Token::Punct(Punct { ch: ':', .. }));
            let ty = Type::parse(stream)?;
            params.push((name, ty));
        }

        let mut open = Span::default();
        let ret_type = match stream.parse()? {
            Token::Punct(Punct { ch: '-', .. }) => {
                expect!(stream, Token::Punct(Punct { ch: '>', .. }));
                let tmp = Type::parse(stream)?;
                open = expect!(stream, Token::Punct(Punct { ch: '{', span }) => span);
                tmp
            }
            Token::Punct(Punct { ch: '{', span }) => {
                open = span;
                Type::empty()
            }
            _ => panic!("unexpected token"),
        };
        Ok((
            Self {
                name,
                params,
                ret_type,
            },
            open,
        ))
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn params(&self) -> impl Iterator<Item = &(Ident, Type)> {
        self.params.iter()
    }

    pub fn label(&self) -> String {
        let mut ret = String::new();
        for (_i, t) in self.params() {
            ret += &t.as_str();
            ret += "_";
        }
        ret += "fn_";
        ret += self.name.as_str();
        ret
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionDef {
        sig: FunctionSignature,
        body: Block,
        span: Span,
    },
    Const {
        name: Ident,
        ty: Type,
        value: Expression,
        span: Span,
    },
    Let {
        name: Ident,
        ty: Option<Type>,
        value: Expression,
        span: Span,
    },
    While {
        condition: Expression,
        body: Block,
        span: Span,
    },
    For {
        vars: Vec<Expression>,
        iter: Expression,
        body: Block,
        span: Span,
    },
    Return {
        expr: Expression,
        span: Span,
    },
    Break {
        expr: Expression,
        span: Span,
    },
    Continue {
        expr: Expression,
        span: Span,
    },
    Block(Block),
    If(IfChain),
    Implicit(Expression),
    FnCall(FnCall, Span),
    Empty,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    //pub scope: LocalScope,
    span: Span,
}

impl Block {
    pub fn parse<R: BufRead>(
        stream: &mut TokenStream<R>,
        mut start: Span,
    ) -> std::io::Result<Self> {
        let mut statements = vec![];
        loop {
            match stream.peek()? {
                Token::Punct(Punct { ch: '}', span }) => {
                    start = start.union(span);
                    break;
                }
                _ => statements.push(Statement::parse(stream)?),
            }
        }
        stream.parse()?;
        Ok(Self {
            statements,
            span: start,
        })
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl Statement {
    pub fn parse<R: BufRead>(stream: &mut TokenStream<R>) -> std::io::Result<Self> {
        match stream.parse()? {
            Token::Ident(Ident::Fun(s)) => {
                let (sig, open) = FunctionSignature::parse(stream)?;
                let body = Block::parse(stream, open)?;
                Ok(Self::FunctionDef {
                    span: s.union(&body.span()),
                    sig,
                    body,
                })
            }
            Token::Ident(Ident::Let(s)) => {
                let name = expect!(stream, Token::Ident(i) if !i.is_keyword() => i);
                let ty = match stream.parse()? {
                    Token::Punct(Punct { ch: '=', .. }) => None,
                    Token::Punct(Punct { ch: ':', .. }) => Some(Type::parse(stream)?),
                    _ => panic!("Unexpected token"),
                };
                if ty.is_some() {
                    expect!(stream, Token::Punct(Punct { ch: '=', .. }));
                }
                let (value, span) = Expression::parse(stream, ';')?;
                Ok(Self::Let {
                    name,
                    ty,
                    value,
                    span: s.union(&span),
                })
            }
            Token::Ident(Ident::Const(s)) => {
                let name = expect!(stream, Token::Ident(i) if !i.is_keyword() => i);
                expect!(stream, Token::Punct(Punct { ch: ':', .. }));
                let ty = Type::parse(stream)?;
                expect!(stream, Token::Punct(Punct { ch: '=', .. }));
                let (value, span) = Expression::parse(stream, ';')?;
                Ok(Self::Const {
                    name,
                    ty,
                    value,
                    span: s.union(&span),
                })
            }
            Token::Ident(Ident::Return(s)) => {
                let (expr, _) = Expression::parse(stream, ';')?;
                Ok(Self::Return {
                    span: s.union(&expr.span()),
                    expr,
                })
            }
            Token::Ident(Ident::Break(s)) => {
                let (expr, _) = Expression::parse(stream, ';')?;
                Ok(Self::Break {
                    span: s.union(&expr.span()),
                    expr,
                })
            }
            Token::Ident(Ident::Continue(s)) => {
                let (expr, _) = Expression::parse(stream, ';')?;
                Ok(Self::Continue {
                    span: s.union(&expr.span()),
                    expr,
                })
            }
            Token::Ident(Ident::While(s)) => {
                let (condition, span) = Expression::parse(stream, '{')?;
                let body = Block::parse(stream, span)?;
                Ok(Self::While {
                    span: s.union(&body.span()),
                    condition,
                    body,
                })
            }
            Token::Ident(Ident::For(s)) => {
                let (vars, _) = Expression::parse(stream, 'i')?;
                let (iter, span) = Expression::parse(stream, '{')?;
                let body = Block::parse(stream, span)?;
                Ok(Self::For {
                    span: s.union(&body.span()),
                    vars: vars.to_tuple(),
                    iter,
                    body,
                })
            }
            Token::Ident(Ident::If(s)) => {
                let (condition, span) = Expression::parse(stream, '{')?;
                let body = Block::parse(stream, span)?;
                let mut next = None;
                let mut cur = &mut next;
                while let Token::Ident(Ident::Else(s)) = stream.peek()? {
                    let s = *s;
                    stream.parse()?; // Toss else
                    match stream.parse()? {
                        Token::Ident(Ident::If(s)) => {
                            let (condition, span) = Expression::parse(stream, '{')?;
                            let body = Block::parse(stream, span)?;
                            *cur = Some(Box::new(IfChain::If {
                                span: s.union(&body.span()),
                                body,
                                condition: Box::new(condition),
                                next: None,
                            }));
                            cur = if let IfChain::If { next, .. } = cur.as_mut().unwrap().as_mut() {
                                next
                            } else {
                                unreachable!()
                            };
                        }
                        token => {
                            let span = token.span();
                            let body = Block::parse(stream, span)?;
                            *cur = Some(Box::new(IfChain::Else {
                                span: s.union(&body.span()),
                                body,
                            }));
                        }
                    }
                }
                Ok(Self::If(IfChain::If {
                    condition: Box::new(condition),
                    span: s.union(&body.span()),
                    body,
                    next,
                }))
            }
            Token::Punct(Punct { ch: '{', span }) => Ok(Self::Block(Block::parse(stream, span)?)),
            Token::EOF => Ok(Self::Empty),
            token => {
                //println!("{:?}", token);
                stream.un_peek(token);
                let (expression, _span, is_implicit) = Expression::parse_implicit(stream, ';')?;
                if is_implicit {
                    Ok(Self::Implicit(expression))
                } else if let Expression::FnCall(f, s) = expression {
                    Ok(Self::FnCall(f, s))
                } else {
                    panic!("Only fncalls may be their own statement")
                }
            }
        }
    }
}

pub struct StatementStream<R: BufRead> {
    inner: TokenStream<R>,
}

impl<R: BufRead> From<TokenStream<R>> for StatementStream<R> {
    fn from(inner: TokenStream<R>) -> Self {
        Self { inner }
    }
}

impl<R: BufRead> Iterator for StatementStream<R> {
    type Item = Statement;
    fn next(&mut self) -> Option<Self::Item> {
        match Statement::parse(&mut self.inner).ok() {
            Some(Statement::Empty) => None,
            a => a,
        }
    }
}
