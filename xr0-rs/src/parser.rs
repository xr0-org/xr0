use crate::ast::Ast;

mod env;
mod inner;

use env::Env;
pub use env::{lexememarker_copy, lexememarker_destroy, lexememarker_str, LexemeMarker};

pub fn parse_translation_unit(
    source: &str,
) -> Result<*mut Ast, peg::error::ParseError<peg::str::LineCol>> {
    let env = Env::new();
    inner::c_parser::translation_unit(source, &env)
}
