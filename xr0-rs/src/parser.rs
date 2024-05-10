use std::path::Path;

use crate::ast::Ast;

mod env;
mod gram;

use env::Env;
pub use env::LexemeMarker;

pub fn parse_translation_unit(
    filename: &Path,
    source: &str,
) -> Result<Box<Ast>, peg::error::ParseError<peg::str::LineCol>> {
    let env = Env::new(filename, source);
    gram::c_parser::translation_unit(source, &env)
}
