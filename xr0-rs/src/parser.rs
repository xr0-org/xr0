#![allow(non_camel_case_types, dead_code)]

use crate::ast::ast;

mod inner;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct lexememarker {
    pub linenum: libc::c_int,
    pub column: libc::c_int,
    pub filename: *mut libc::c_char,
    pub flags: linemarker_flag,
}
pub type linemarker_flag = libc::c_uint;
pub const LM_FLAG_IMPLICIT_EXTERN: linemarker_flag = 8;
pub const LM_FLAG_SYS_HEADER: linemarker_flag = 4;
pub const LM_FLAG_RESUME_FILE: linemarker_flag = 2;
pub const LM_FLAG_NEW_FILE: linemarker_flag = 1;

#[link(name = "xr0parse", kind = "static")]
extern "C" {
    pub fn lex_begin();
    pub fn lex_finish();
    pub fn lexememarker_copy(loc: *mut lexememarker) -> *mut lexememarker;
    pub fn lexememarker_destroy(loc: *mut lexememarker);
    pub fn lexememarker_str(loc: *mut lexememarker) -> *mut libc::c_char;
    pub fn yylex_destroy() -> libc::c_int;

    pub static mut yyin: *mut libc::FILE;
    // pub static mut yychar: libc::c_int;
    // pub static mut yylval: YYSTYPE;
    // pub static mut yynerrs: libc::c_int;
    pub fn yyparse() -> libc::c_int;
}

static RESERVED: &[&str] = &[
    "auto", "axiom", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "int", "lemma", "long", "register",
    "return", "short", "signed", "sizeof", "some", "static", "sfunc", "struct", "switch",
    "typedef", "union", "unsigned", "void", "volatile", "while",
];

pub fn parse_translation_unit(
    source: &str,
) -> Result<*mut ast, peg::error::ParseError<peg::str::LineCol>> {
    let env = inner::Env {
        reserved: RESERVED.iter().copied().collect(),
    };
    inner::c_parser::translation_unit(source, &env)
}
