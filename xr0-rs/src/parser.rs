#![allow(non_camel_case_types, dead_code)]

use crate::ast::ast;

mod env;
mod inner;

use env::Env;
pub use env::{lexememarker, lexememarker_copy, lexememarker_destroy, lexememarker_str};

pub fn parse_translation_unit(
    source: &str,
) -> Result<*mut ast, peg::error::ParseError<peg::str::LineCol>> {
    let env = Env::new();
    inner::c_parser::translation_unit(source, &env)
}
