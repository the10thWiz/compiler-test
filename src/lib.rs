use crate::statement::Statement;

mod expression;
mod scope;
mod statement;
mod token;
mod types;

pub const EXAMPLE_SOURCE: &str = "
    // test
    fn main(b: &mut u64) {
        if a > b {
            let a: u64 = b.abs() + 12;
        }
        printf(\"%n\", a);
    }
";

pub fn main() -> std::io::Result<()> {
    println!("{}", EXAMPLE_SOURCE);
    let mut stream = token::TokenStream::new(EXAMPLE_SOURCE.as_bytes());
    while let Ok(statement) = Statement::parse(&mut stream) {
        if let Statement::Empty = statement {
            break;
        }
        println!("{:#?}", statement);
    }
    //for token in stream {
    //println!(
    //"{} -> {:?}",
    //&EXAMPLE_SOURCE[token.span().as_range()],
    //token
    //);
    //}
    Ok(())
}
