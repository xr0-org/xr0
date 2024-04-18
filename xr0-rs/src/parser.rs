use std::path::Path;

use crate::ast::Ast;

mod env;
mod inner;

use env::Env;
pub use env::{lexememarker_copy, lexememarker_destroy, lexememarker_str, LexemeMarker};

pub fn parse_translation_unit(
    filename: &Path,
    source: &str,
) -> Result<Box<Ast>, peg::error::ParseError<peg::str::LineCol>> {
    let env = Env::new(filename, source);
    inner::c_parser::translation_unit(source, &env)
}
