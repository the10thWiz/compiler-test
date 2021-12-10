use std::io::BufRead;

use crate::statement::Block;
use crate::statement::FunctionSignature;
use crate::token::Ident;
use crate::token::Literal;
use crate::token::Punct;
use crate::token::Span;
use crate::token::Token;
use crate::token::TokenStream;
use crate::EXAMPLE_SOURCE;

/// There is no ElseIf variant, since it would be the same as an if. The only way to identify if an
/// If is actually an IfElse is whether it's in the master `Statement::If` or in the next of an
/// `IfChain::If`
#[derive(Debug, Clone)]
pub enum IfChain {
    If {
        condition: Box<Expression>,
        body: Block,
        next: Option<Box<IfChain>>,
        span: Span,
    },
    Else {
        body: Block,
        span: Span,
    },
}

impl IfChain {
    pub fn span(&self) -> Span {
        match self {
            Self::If { span, .. } => *span,
            Self::Else { span, .. } => *span,
        }
    }
    pub fn set_span(&mut self, new_span: Span) {
        match self {
            Self::If { span, .. } => *span = new_span,
            Self::Else { span, .. } => *span = new_span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnCall {
    name: Box<Expression>,
    params: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    FnCall(FnCall, Span),
    Value(Literal),
    Variable(Ident),
    IfChain(IfChain),
    Binary(Box<Expression>, Operation, Box<Expression>),
    Unary(Operation, Box<Expression>),
    Tuple(Vec<Expression>, Span),
}

#[derive(Debug, Clone)]
enum PartialExpression {
    Token(Punct),
    Operation(Operation),
    Expression(Expression),
    Parens(Expression),
    Square(Expression),
    Curly(Block),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    Addition(Span),
    Subtraction(Span),
    Multiplication(Span),
    Division(Span),
    Modulo(Span),
    Assignment(Span),
    Equality(Span),
    Not(Span),
    InEquality(Span),
    LessThan(Span),
    GreaterThan(Span),
    LessEq(Span),
    GreaterEq(Span),
    Comma(Span),
    Dot(Span),
    Ampersand(Span),
    QuestionMark(Span),
    Colon(Span),
    Namespace(Span),
    Pipe(Span),
    And(Span),
    Or(Span),
    Xor(Span),
    ShiftLeft(Span),
    ShiftRight(Span),

    AdditionAssign(Span),
    SubtractionAssign(Span),
    MultiplicationAssign(Span),
    DivisionAssign(Span),
    ModuloAssign(Span),
    AndAssign(Span),
    OrAssign(Span),
    XorAssign(Span),
}

impl Operation {
    fn span(&self) -> Span {
        match self {
            Self::Addition(span)
            | Self::Subtraction(span)
            | Self::Multiplication(span)
            | Self::Division(span)
            | Self::Modulo(span)
            | Self::Assignment(span)
            | Self::Equality(span)
            | Self::Not(span)
            | Self::InEquality(span)
            | Self::LessThan(span)
            | Self::GreaterThan(span)
            | Self::LessEq(span)
            | Self::GreaterEq(span)
            | Self::Comma(span)
            | Self::Dot(span)
            | Self::Ampersand(span)
            | Self::QuestionMark(span)
            | Self::Colon(span)
            | Self::Namespace(span)
            | Self::Pipe(span)
            | Self::And(span)
            | Self::Or(span)
            | Self::Xor(span)
            | Self::AdditionAssign(span)
            | Self::SubtractionAssign(span)
            | Self::MultiplicationAssign(span)
            | Self::DivisionAssign(span)
            | Self::ModuloAssign(span)
            | Self::AndAssign(span)
            | Self::OrAssign(span)
            | Self::XorAssign(span)
            | Self::ShiftLeft(span)
            | Self::ShiftRight(span) => *span,
        }
    }
}

impl PartialExpression {
    fn combine_ops(collection: &mut Vec<Self>) {
        use Operation::*;
        use PartialExpression::{Operation as Op, Token};
        collection.iter_mut().for_each(|s| match s {
            Token(Punct { ch: '+', span }) => *s = Op(Addition(*span)),
            Token(Punct { ch: '-', span }) => *s = Op(Subtraction(*span)),
            Token(Punct { ch: '*', span }) => *s = Op(Multiplication(*span)),
            Token(Punct { ch: '/', span }) => *s = Op(Division(*span)),
            Token(Punct { ch: '%', span }) => *s = Op(Modulo(*span)),
            Token(Punct { ch: '.', span }) => *s = Op(Dot(*span)),
            Token(Punct { ch: '=', span }) => *s = Op(Assignment(*span)),
            Token(Punct { ch: '!', span }) => *s = Op(Not(*span)),
            Token(Punct { ch: ',', span }) => *s = Op(Comma(*span)),
            Token(Punct { ch: '<', span }) => *s = Op(LessThan(*span)),
            Token(Punct { ch: '>', span }) => *s = Op(GreaterThan(*span)),
            Token(Punct { ch: '&', span }) => *s = Op(Ampersand(*span)),
            Token(Punct { ch: '|', span }) => *s = Op(Pipe(*span)),
            Token(Punct { ch: '^', span }) => *s = Op(Xor(*span)),
            Token(Punct { ch: '?', span }) => *s = Op(QuestionMark(*span)),
            Token(Punct { ch: ':', span }) => *s = Op(Colon(*span)),
            _ => (),
        });
        if collection.len() > 0 {
            let mut i = collection.len() - 1;
            while i > 1 {
                if let Some(op) = match (&collection[i - 1], &collection[i]) {
                    (Op(Assignment(s)), Op(Assignment(s2))) => Some(Equality(s.union(s2))),
                    (Op(Not(s)), Op(Assignment(s2))) => Some(InEquality(s.union(s2))),
                    (Op(LessThan(s)), Op(Assignment(s2))) => Some(LessEq(s.union(s2))),
                    (Op(GreaterThan(s)), Op(Assignment(s2))) => Some(GreaterEq(s.union(s2))),
                    (Op(Ampersand(s)), Op(Ampersand(s2))) => Some(And(s.union(s2))),
                    (Op(Pipe(s)), Op(Pipe(s2))) => Some(Or(s.union(s2))),
                    (Op(LessThan(s)), Op(LessThan(s2))) => Some(ShiftLeft(s.union(s2))),
                    (Op(GreaterThan(s)), Op(GreaterThan(s2))) => Some(ShiftRight(s.union(s2))),
                    (Op(Colon(s)), Op(Colon(s2))) => Some(Namespace(s.union(s2))),
                    (Op(Addition(s)), Op(Assignment(s2))) => Some(AdditionAssign(s.union(s2))),
                    (Op(Subtraction(s)), Op(Assignment(s2))) => {
                        Some(SubtractionAssign(s.union(s2)))
                    }
                    (Op(Multiplication(s)), Op(Assignment(s2))) => {
                        Some(MultiplicationAssign(s.union(s2)))
                    }
                    (Op(Division(s)), Op(Assignment(s2))) => Some(DivisionAssign(s.union(s2))),
                    (Op(Modulo(s)), Op(Assignment(s2))) => Some(ModuloAssign(s.union(s2))),
                    (Op(And(s)), Op(Assignment(s2))) => Some(AndAssign(s.union(s2))),
                    (Op(Or(s)), Op(Assignment(s2))) => Some(OrAssign(s.union(s2))),
                    (Op(Xor(s)), Op(Assignment(s2))) => Some(XorAssign(s.union(s2))),
                    _ => None,
                } {
                    collection.remove(i - 1);
                    collection[i - 1] = Op(op);
                }
                i -= 1;
            }
        }
    }
    fn fn_call(collection: &mut Vec<Self>) {
        if collection.len() > 1 {
            let mut i = collection.len() - 1;
            while i >= 1 {
                if collection[i - 1].is_expression() && collection[i].is_parens() {
                    let params = collection.remove(i).as_expression().unwrap();
                    let span = params.span();
                    let params = params.to_tuple();
                    let name = Box::new(collection[i - 1].clone().as_expression().unwrap());
                    let span = span.union(&name.span());
                    collection[i - 1] =
                        Self::Expression(Expression::FnCall(FnCall { name, params }, span));
                }
                i -= 1;
            }
        }
    }
    fn if_chain(collection: &mut Vec<Self>) {
        let mut i = 0;
        while i < collection.len() {
            if let Self::Expression(Expression::Variable(Ident::If(s))) = &collection[i] {
                let s = *s;
                let condition = collection
                    .remove(i + 1)
                    .as_expression()
                    .expect("If condition must be expression");
                let block = collection
                    .remove(i + 1)
                    .as_block()
                    .expect("If blocks must have curly braces");
                let mut next = None;
                let mut cur = &mut next;
                while let Self::Expression(Expression::Variable(Ident::Else(s))) =
                    &collection[i + 1]
                {
                    let s = *s;
                    collection.remove(i + 1);
                    match collection.remove(i + 1) {
                        Self::Expression(Expression::Variable(Ident::If(s))) => {
                            collection.remove(i + 1); // remove if
                            let condition = collection
                                .remove(i + 1)
                                .as_expression()
                                .expect("If condition must be expression");
                            let block = collection
                                .remove(i + 1)
                                .as_block()
                                .expect("If blocks must have curly braces");
                            *cur = Some(Box::new(IfChain::If {
                                span: s.union(&block.span()),
                                condition: Box::new(condition),
                                body: block,
                                next: None,
                            }));
                            cur = if let IfChain::If { next, .. } = cur.as_mut().unwrap().as_mut() {
                                next
                            } else {
                                unreachable!()
                            };
                        }
                        Self::Curly(block) => {
                            *cur = Some(Box::new(IfChain::Else {
                                span: s.union(&block.span()),
                                body: block,
                            }))
                        }
                        _ => panic!("unexpect token after else"),
                    }
                }
                collection[i] = Self::Expression(Expression::IfChain(IfChain::If {
                    span: s.union(&block.span()),
                    condition: Box::new(condition),
                    body: block,
                    next,
                }));
            }
            i += 1;
        }
    }
    fn binary_ops(collection: &mut Vec<Self>, ops: impl Fn(Operation) -> bool) {
        if collection.len() > 2 {
            let mut i = collection.len() - 1;
            while i >= 1 {
                if let Self::Operation(o) = collection[i - 1] {
                    if ops(o) && collection[i - 2].is_expression() && collection[i].is_expression()
                    {
                        let rhs = collection.remove(i).as_expression().unwrap();
                        let lhs = collection.remove(i - 2).as_expression().unwrap();
                        collection[i - 2] =
                            Self::Expression(Expression::Binary(Box::new(lhs), o, Box::new(rhs)));
                        i -= 1;
                    }
                }
                i -= 1;
            }
        }
    }
    fn unary_ops(collection: &mut Vec<Self>, ops: impl Fn(Operation) -> bool) {
        if collection.len() > 1 {
            let mut i = collection.len() - 1;
            while i >= 1 {
                if let (Self::Operation(o), Self::Expression(_)) =
                    (&collection[i - 1], &collection[i])
                {
                    let o = *o;
                    if ops(o) {
                        if let Self::Expression(e) = collection.remove(i) {
                            collection[i - 1] = Self::Expression(Expression::Unary(o, Box::new(e)));
                        } else {
                            unreachable!()
                        }
                    }
                }
                i -= 1;
            }
        }
    }

    fn as_expression(self) -> Option<Expression> {
        match self {
            Self::Expression(e) | Self::Square(e) | Self::Parens(e) => Some(e),
            _ => None,
        }
    }
    fn is_expression(&self) -> bool {
        match self {
            Self::Expression(_e) | Self::Square(_e) | Self::Parens(_e) => true,
            _ => false,
        }
    }
    fn is_parens(&self) -> bool {
        match self {
            Self::Parens(_) => true,
            _ => false,
        }
    }
    fn as_block(self) -> Option<Block> {
        match self {
            Self::Curly(b) => Some(b),
            _ => None,
        }
    }
}

impl Expression {
    pub fn parse<R: BufRead>(
        stream: &mut TokenStream<R>,
        end: char,
    ) -> std::io::Result<(Self, Span)> {
        Self::parse_implicit(stream, end).map(|(e, s, _)| (e, s))
    }
    pub fn parse_implicit<R: BufRead>(
        stream: &mut TokenStream<R>,
        end: char,
    ) -> std::io::Result<(Self, Span, bool)> {
        //println!("{}", &EXAMPLE_SOURCE[stream.get_pos()..]);
        use Operation::*;
        let (mut partial, span, is_implicit) = Self::collect(stream, end)?;
        PartialExpression::combine_ops(&mut partial);
        PartialExpression::binary_ops(&mut partial, |o| matches!(o, Dot(_) | Namespace(_)));
        PartialExpression::fn_call(&mut partial);
        PartialExpression::unary_ops(&mut partial, |o| {
            matches!(o, Ampersand(_) | Multiplication(_))
        });
        PartialExpression::unary_ops(&mut partial, |o| matches!(o, Subtraction(_) | Not(_)));
        PartialExpression::binary_ops(&mut partial, |o| {
            matches!(o, Multiplication(_) | Division(_) | Modulo(_))
        });
        PartialExpression::binary_ops(&mut partial, |o| matches!(o, Ampersand(_) | Pipe(_)));
        PartialExpression::binary_ops(&mut partial, |o| matches!(o, Addition(_) | Subtraction(_)));
        PartialExpression::binary_ops(&mut partial, |o| {
            matches!(
                o,
                LessThan(_)
                    | LessEq(_)
                    | GreaterThan(_)
                    | GreaterEq(_)
                    | Equality(_)
                    | InEquality(_)
            )
        });
        PartialExpression::binary_ops(&mut partial, |o| matches!(o, And(_) | Or(_) | Xor(_)));
        PartialExpression::binary_ops(&mut partial, |o| {
            matches!(
                o,
                AdditionAssign(_)
                    | SubtractionAssign(_)
                    | MultiplicationAssign(_)
                    | DivisionAssign(_)
                    | ModuloAssign(_)
                    | AndAssign(_)
                    | OrAssign(_)
                    | XorAssign(_)
            )
        });
        PartialExpression::if_chain(&mut partial);
        //println!("{:?}", partial);
        if partial.len() == 1 {
            if let PartialExpression::Expression(e) = partial.remove(0) {
                Ok((e, span, is_implicit))
            } else {
                panic!("Not fully simplified");
            }
        } else {
            let mut ret = vec![];
            while partial.len() > 0 {
                if let Some(e) = partial.remove(0).as_expression() {
                    ret.push(e);
                    if partial.len() > 0
                        && !matches!(
                            partial.remove(0),
                            PartialExpression::Operation(Operation::Comma(_))
                        )
                    {
                        panic!("Not fully simplified {:?}", partial)
                    }
                }
            }
            Ok((Self::Tuple(ret, Span::default()), span, is_implicit))
        }
    }

    fn collect<R: BufRead>(
        stream: &mut TokenStream<R>,
        end: char,
    ) -> std::io::Result<(Vec<PartialExpression>, Span, bool)> {
        let mut ret = vec![];
        let mut is_implicit = false;
        let mut next = Span::default();
        loop {
            match stream.parse()? {
                Token::Punct(Punct { ch, span }) if ch == end => {
                    next = span;
                    break;
                }
                Token::Ident(Ident::In(span)) if end == 'i' => {
                    next = span;
                    break;
                }
                token @ Token::Punct(Punct { ch: '}', .. }) if end == ';' => {
                    stream.un_peek(token);
                    is_implicit = true;
                    break;
                }
                Token::Punct(Punct { ch: '(', span }) => {
                    let (exp, end) = Self::parse(stream, ')')?;
                    ret.push(PartialExpression::Parens(exp.with_span(span.union(&end))))
                }
                Token::Punct(Punct { ch: '[', span }) => {
                    let (exp, end) = Self::parse(stream, ']')?;
                    ret.push(PartialExpression::Square(exp.with_span(span.union(&end))))
                }
                Token::Punct(Punct { ch: '{', span }) => {
                    ret.push(PartialExpression::Curly(Block::parse(stream, span)?))
                }
                Token::Ident(i) => ret.push(PartialExpression::Expression(Expression::Variable(i))),
                Token::Literal(l) => ret.push(PartialExpression::Expression(Expression::Value(l))),
                Token::Punct(token) => ret.push(PartialExpression::Token(token)),
                _ => todo!("{:?}", stream),
            }
        }
        Ok((ret, next, is_implicit))
    }

    pub fn to_tuple(self) -> Vec<Self> {
        match self {
            Self::Tuple(e, _) => e,
            e => vec![e],
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::FnCall(_, s) => *s,
            Self::Value(v) => v.span(),
            Self::Variable(v) => v.span(),
            Self::IfChain(ch) => ch.span(),
            Self::Binary(a, _, b) => a.span().union(&b.span()),
            Self::Unary(a, b) => a.span().union(&b.span()),
            Self::Tuple(_, s) => *s,
        }
    }

    pub fn set_span(&mut self, span: Span) {
        match self {
            Self::FnCall(_, s) => *s = span,
            Self::Value(v) => v.set_span(span),
            Self::Variable(v) => v.set_span(span),
            Self::IfChain(ch) => ch.set_span(span),
            Self::Binary(_, _, _) => unimplemented!(),
            Self::Unary(_, _) => unimplemented!(),
            Self::Tuple(_, s) => *s = span,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.set_span(span);
        self
    }
}
