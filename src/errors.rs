use crate::token::{Span, Token};

//
// errors.rs
// Copyright (C) 2021 matthew <matthew@WINDOWS-05HIC4F>
// Distributed under terms of the MIT license.
//

#[derive(Debug, Clone)]
pub enum CompilerError {
    UnexpectedToken(Token),
}

impl CompilerError {
    pub fn span(&self) -> Span {
        match self {
            Self::UnexpectedToken(t) => t.span(),
        }
    }

    pub fn region<'a>(&self, source: &'a str) -> &'a str {
        &source[self.span().as_range()]
    }

    pub fn pretty_print(&self, source: &str) {
        match self {
            Self::UnexpectedToken(t) => println!("Unexpected token: {:?}", t),
        }
        let Span { start, end } = self.span();
        let line_start = source[..start].rfind('\n').unwrap_or(0);
        let line_end = source[end..].find('\n').unwrap_or(source.len());
        println!("{}", &source[line_start..line_end]);
        for _ in line_start..start {
            print!(" ");
        }
        for _ in start..end {
            print!("^");
        }
        println!();
    }
}
