pub use crate::{scope::Scope, statement::StatementStream};

#[macro_use]
mod asm;
mod errors;
mod expression;
mod scope;
mod statement;
mod token;
mod types;

pub const EXAMPLE_SOURCE: &str = "
    fn printf(s: u64, c: u64) {}
    // test
    fn main(b: &mut u64) {
        let c: u64 = 12;
        if c > b {
            //let a: u64 = b.abs() + 12;
        }
        printf(\"%n\", c);
    }
";

pub fn main() -> std::io::Result<()> {
    println!("{}", EXAMPLE_SOURCE);
    let stream = token::TokenStream::new(EXAMPLE_SOURCE.as_bytes());
    let scope = Scope::from_statements(StatementStream::from(stream));
    println!("{:#?}", scope);
    //while let Ok(statement) = Statement::parse(&mut stream) {
    //if let Statement::Empty = statement {
    //break;
    //}
    //println!("{:#?}", statement);
    //}
    //for token in stream {
    //println!(
    //"{} -> {:?}",
    //&EXAMPLE_SOURCE[token.span().as_range()],
    //token
    //);
    //}
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        statement::{FunctionSignature, Statement},
        token::{Ident, Span, TokenStream},
        types::Type,
    };

    use super::*;

    macro_rules! ast {
        ($scope:expr, $src:literal) => {{
            const EXAMPLE: &str = $src;
            let stream = token::TokenStream::new(EXAMPLE.as_bytes());
            //let scope = Scope::from_statements(StatementStream::from(stream));
            $scope.parse_statements(StatementStream::from(stream));
        }};
        (fn $name:ident ($($param:ident : $ty:literal),*) -> $ret:literal) => {
            FunctionSignature {
                name: token::Ident::User {
                    val: stringify!($name).to_string(),
                    span: Span::default(),
                },
                params: vec![$((
                    token::Ident::User {
                        val: stringify!($param).to_string(),
                        span: Span::default(),
                    }, Type::parse(&mut TokenStream::new($ty.as_bytes())).expect("Failed to parse type")
                )),*],
                ret_type: Type::parse(&mut TokenStream::new($ret.as_bytes())).expect("Failed to parse type"),
            }
        };
    }

    #[test]
    fn empty_fn() {
        let mut scope = Scope::default();
        ast!(scope, " fn main() {} ");
        let main = ast!(fn main() -> "()");
        assert_eq!(
            scope.global.functions[0].0, main,
            "Did not parse main fn correctly"
        );
    }

    macro_rules! vec_match {
        ($vec:expr, [$($parsed:pat $(if $cond:expr)?),*]) => {{
            #[allow(unused_mut)]
            let mut tmp = $vec;
            $(
                if let $parsed = tmp.remove(0) {
                    $(
                        if !($cond) {
                            panic!("Expression parsing failed");
                        }
                     )?
                } else {
                    panic!("Expression parsing failed");
                }
            )*
            if tmp.len() != 0 {
                panic!("Too many expressions");
            }
            true
        }};
    }

    #[test]
    fn statements() {
        macro_rules! test {
            ($src:literal => $($parsed:pat $(if $cond:expr)?),+) => {{
                const EXAMPLE: &str = $src;
                let stream = token::TokenStream::new(EXAMPLE.as_bytes());
                let mut tmp = StatementStream::from(stream).collect::<Vec<_>>();
                $(
                    if let $parsed = tmp.remove(0) {
                        $(
                            if !($cond) {
                                panic!("Expression parsing failed");
                            }
                        )?
                    } else {
                        panic!("Expression parsing failed");
                    }
                )+
            }};
        }
        test!(" fn main() {} " => Statement::FunctionDef {
            sig: FunctionSignature {
                    name: Ident::User {val: name, ..},
                    params,
                    ret_type
                },
            body,
            ..
        } if name == "main" && params == vec![] && ret_type == Type::empty() && vec_match!(body.statements, []));
        test!(" fn printf() {} " => Statement::FunctionDef {
            sig: FunctionSignature {
                    name: Ident::User {val: name, ..},
                    params,
                    ret_type
                },
            body,
            ..
        } if name == "printf" && params == vec![] && ret_type == Type::empty() && vec_match!(body.statements, []));
        test!(" fn main(a: u64) {} " => Statement::FunctionDef {
            sig: FunctionSignature {
                    name: Ident::User {val: name, ..},
                    params,
                    ret_type
                },
            body,
            ..
        } if name == "main" && vec_match!(params, [(_, Type::Primitive {
            floating: false,
            signed: false,
            size: 8,
            mutable: false,
            ..
        })]) && ret_type == Type::empty() && vec_match!(body.statements, []));
        test!(" fn main(a: i64) {} " => Statement::FunctionDef {
            sig: FunctionSignature {
                    name: Ident::User {val: name, ..},
                    params,
                    ret_type
                },
            body,
            ..
        } if name == "main" && vec_match!(params, [(_, Type::Primitive {
            floating: false,
            signed: true,
            size: 8,
            mutable: false,
            ..
        })]) && ret_type == Type::empty() && vec_match!(body.statements, []));
    }
}
