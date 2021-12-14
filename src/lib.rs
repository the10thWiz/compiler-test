pub use crate::{scope::Scope, statement::StatementStream};

#[macro_use]
mod asm;
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
        statement::FunctionSignature,
        token::{Span, TokenStream},
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
}
