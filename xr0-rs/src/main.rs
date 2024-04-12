#![feature(c_variadic)]

use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};
use std::{env, ptr};

use clap::Parser;
use libc::{exit, fprintf, strcmp};

mod util;

mod ast;
mod c_util;
mod ext;
mod math;
mod object;
mod parser;
mod props;
mod state;
mod value;

use ast::{
    ast as Ast, ast_block, ast_block_str, ast_destroy, ast_expr as AstExpr, ast_externdecl,
    ast_externdecl_as_function, ast_externdecl_install, ast_externdecl_isfunction, ast_function,
    ast_function_absisempty, ast_function_abstract, ast_function_copy, ast_function_isaxiom,
    ast_function_isproto, ast_function_name, ast_function_verify, ast_functiondecl_create,
    ast_protostitch, ast_topological_order, ast_type, ast_variable, ast_variable_arr,
};
use c_util::__stderrp;
use ext::{externals as Externals, externals_create, externals_destroy, externals_getfunc};
use math::math_expr;
use object::{object as Object, object_arr};
use props::props as Props;
use state::block::{block as Block, block_arr};
use state::clump::clump as Clump;
use state::heap::{heap as Heap, vconst};
use state::location::location as Location;
use state::r#static::static_memory;
use state::stack::{stack as Stack, variable as Variable};
use state::state::state as State;
use util::{
    strbuilder as StrBuilder, string_arr, string_arr_n, string_arr_s, string_arr_str, VERBOSE_MODE,
};
use value::value as Value;

#[allow(non_camel_case_types)]
pub type sortmode = libc::c_uint;
pub const SORTMODE_VERIFY: sortmode = 2;
pub const SORTMODE_SORT: sortmode = 1;
pub const SORTMODE_NONE: sortmode = 0;

#[derive(Parser)]
pub struct Config {
    #[arg(value_name = "FILE")]
    pub infile: PathBuf,

    #[arg(short, default_value = "0.c")]
    pub outfile: PathBuf,

    #[arg(short = 'I', action = clap::ArgAction::Append)]
    pub include_dirs: Vec<PathBuf>,

    #[arg(short, long)]
    pub verbose: bool,

    /// Function to evaluate dependencies for.
    #[arg(short = 't', group = "sortmode")]
    pub sort: Option<String>,

    #[arg(short = 'x', group = "sortmode")]
    pub verify: Option<String>,

    #[arg(short = 's', long = "strip")]
    pub strip_mode: bool,
}

/*
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sortconfig {
    pub mode: sortmode,
    pub sortfunc: *mut libc::c_char,
}

impl Config {
    fn sortmode(&self) -> sortconfig {
        if let Some(sortfunc) = &self.sort {
            let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
            sortconfig {
                mode: SORTMODE_SORT,
                sortfunc: sortfunc_cstr.into_raw(),
            }
        } else if let Some(sortfunc) = &self.verify {
            let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
            sortconfig {
                mode: SORTMODE_VERIFY,
                sortfunc: sortfunc_cstr.into_raw(),
            }
        } else {
            sortconfig {
                mode: SORTMODE_NONE,
                sortfunc: &(0 as libc::c_char),
            }
        }
    }
}
 */

pub fn preprocess(infile: &Path, include_dirs: &[PathBuf]) -> io::Result<String> {
    let mut cmd = Command::new("cc");
    for path in include_dirs {
        cmd.arg("-I").arg(path);
    }
    let mut process = cmd
        .arg("-nostdinc")
        .arg("-E")
        .arg("-xc")
        .arg(infile)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()?;

    let mut buf = String::new();
    process.stdout.take().unwrap().read_to_string(&mut buf)?;

    let status = process.wait()?;
    if !status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            format!("failed to run C preprocessor: {status}"),
        ));
    }

    Ok(buf)
}

#[allow(non_upper_case_globals)]
pub static mut root: *mut Ast = 0 as *const Ast as *mut Ast;

pub unsafe fn pass0(root_0: *mut Ast, ext: *mut Externals) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*root_0).n {
        let decl: *mut ast_externdecl = *((*root_0).decl).offset(i as isize);
        if !ast_externdecl_isfunction(decl) {
            ast_externdecl_install(decl, ext);
        } else {
            let f: *mut ast_function = ast_externdecl_as_function(decl);
            if ast_function_isaxiom(f) {
                ast_externdecl_install(decl, ext);
            } else if ast_function_isproto(f) {
                if !verifyproto(f, (*root_0).n, (*root_0).decl) {
                    exit(1 as libc::c_int);
                }
                ast_externdecl_install(decl, ext);
            } else {
                let stitched: *mut ast_function = ast_protostitch(f, ext);
                ast_externdecl_install(ast_functiondecl_create(ast_function_copy(stitched)), ext);
            }
        }
        i += 1;
    }
}

pub unsafe fn pass1(root_0: *mut Ast, ext: *mut Externals) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*root_0).n {
        let decl: *mut ast_externdecl = *((*root_0).decl).offset(i as isize);
        if ast_externdecl_isfunction(decl) {
            let f: *mut ast_function = ast_externdecl_as_function(decl);
            if !(ast_function_isaxiom(f) as libc::c_int != 0
                || ast_function_isproto(f) as libc::c_int != 0)
            {
                if (ast_function_abstract(f)).is_null() {
                    panic!();
                }
                let err = ast_function_verify(f, ext);
                if !err.is_null() {
                    fprintf(
                        __stderrp,
                        b"%s\n\0" as *const u8 as *const libc::c_char,
                        (*err).msg,
                    );
                    exit(1 as libc::c_int);
                }
                vprintln!(
                    "qed {}",
                    CStr::from_ptr(ast_function_name(f)).to_string_lossy()
                );
            }
        }
        i += 1;
    }
}

pub unsafe fn pass_inorder(order: &mut string_arr, ext: *mut Externals) {
    let n: libc::c_int = string_arr_n(order);
    let name: *mut *mut libc::c_char = string_arr_s(order);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let f: *mut ast_function = externals_getfunc(ext, *name.offset(i as isize));
        if !(ast_function_isaxiom(f) as libc::c_int != 0
            || ast_function_isproto(f) as libc::c_int != 0)
        {
            if (ast_function_abstract(f)).is_null() {
                panic!();
            }
            let err = ast_function_verify(f, ext);
            if !err.is_null() {
                fprintf(
                    __stderrp,
                    b"%s\n\0" as *const u8 as *const libc::c_char,
                    (*err).msg,
                );
                exit(1 as libc::c_int);
            }
            fprintf(
                __stderrp,
                b"qed %s\n\0" as *const u8 as *const libc::c_char,
                ast_function_name(f),
            );
        }
        i += 1;
    }
}

unsafe fn verifyproto(
    proto: *mut ast_function,
    n: libc::c_int,
    // Note: unused parameter in original. Should use this instead of `root->decl`.
    _decl: *mut *mut ast_externdecl,
) -> bool {
    let mut def: *mut ast_function = ptr::null_mut();
    let mut count: libc::c_int = 0 as libc::c_int;
    let pname: *mut libc::c_char = ast_function_name(proto);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let decl_0: *mut ast_externdecl = *((*root).decl).offset(i as isize);
        if ast_externdecl_isfunction(decl_0) {
            let d: *mut ast_function = ast_externdecl_as_function(decl_0);
            if !(ast_function_isaxiom(d) as libc::c_int != 0
                || ast_function_isproto(d) as libc::c_int != 0)
            {
                if strcmp(pname, ast_function_name(d)) == 0 as libc::c_int {
                    def = d;
                    count += 1;
                }
            }
        }
        i += 1;
    }
    if count == 1 as libc::c_int {
        if proto_defisvalid(proto, def) {
            return 1 as libc::c_int != 0;
        }
        fprintf(
            __stderrp,
            b"function `%s' prototype and definition abstracts mismatch\n\0" as *const u8
                as *const libc::c_char,
            pname,
        );
    } else if count == 0 as libc::c_int {
        fprintf(
            __stderrp,
            b"function `%s' missing definition\n\0" as *const u8 as *const libc::c_char,
            pname,
        );
    } else if count > 1 as libc::c_int {
        fprintf(
            __stderrp,
            b"function `%s' has multiple definitions\n\0" as *const u8 as *const libc::c_char,
            pname,
        );
    }
    return 0 as libc::c_int != 0;
}

unsafe fn proto_defisvalid(proto: *mut ast_function, def: *mut ast_function) -> bool {
    let proto_abs: *mut ast_block = ast_function_abstract(proto);
    let def_abs: *mut ast_block = ast_function_abstract(def);
    let abs_match: bool = strcmp(
        ast_block_str(
            proto_abs,
            b"\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ),
        ast_block_str(
            def_abs,
            b"\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ),
    ) == 0 as libc::c_int;
    let protoabs_only: bool =
        !proto_abs.is_null() && ast_function_absisempty(def) as libc::c_int != 0;
    if abs_match as libc::c_int != 0 || protoabs_only as libc::c_int != 0 {
        return 1 as libc::c_int != 0;
    }
    return 0 as libc::c_int != 0;
}

unsafe fn verify(c: &Config) -> io::Result<()> {
    let source = preprocess(&c.infile, &c.include_dirs)?;
    println!("SOURCE\n====\n{source}====\n");
    root = parser::parse_translation_unit(&source)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{err}")))?;

    let ext: *mut Externals = externals_create();
    pass0(root, ext);

    if let Some(sortfunc) = &c.sort {
        let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
        let order = ast_topological_order(sortfunc_cstr.as_ptr() as *mut libc::c_char, ext);
        fprintf(
            __stderrp,
            b"%s\n\0" as *const u8 as *const libc::c_char,
            string_arr_str(&order),
        );
    } else if let Some(sortfunc) = &c.verify {
        let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
        let mut order = ast_topological_order(sortfunc_cstr.as_ptr() as *mut libc::c_char, ext);
        fprintf(
            __stderrp,
            b"%s\n\0" as *const u8 as *const libc::c_char,
            string_arr_str(&order),
        );
        pass_inorder(&mut order, ext);
    } else {
        pass1(root, ext);
    }

    externals_destroy(ext);
    ast_destroy(root);
    Ok(())
}

fn strip(config: &Config) -> Result<(), io::Error> {
    let infile = BufReader::new(File::open(&config.infile)?);
    let mut outfile = BufWriter::new(File::create(&config.outfile)?);

    #[derive(Clone, Copy)]
    enum Mode {
        Copy,
        Squiggle,
        VBlock(usize),
    }

    let mut mode = Mode::Copy;
    for result in infile.bytes() {
        let c = result?;
        match (mode, c) {
            (Mode::Copy, b'~') => {
                mode = Mode::Squiggle;
            }
            (Mode::Copy, c) => {
                outfile.write_all(&[c])?;
            }
            (Mode::Squiggle, b'[') => {
                mode = Mode::VBlock(1);
            }
            (Mode::Squiggle, b'~') => {}
            (Mode::Squiggle, c) => {
                outfile.write_all(&[b'~', c])?;
                mode = Mode::Copy;
            }
            (Mode::VBlock(k), b'[') => {
                mode = Mode::VBlock(k + 1);
            }
            (Mode::VBlock(1), b']') => {
                mode = Mode::Copy;
            }
            (Mode::VBlock(k), b']') => {
                mode = Mode::VBlock(k - 1);
            }
            (Mode::VBlock(_), _) => {}
        }
    }
    Ok(())
}

pub fn main() {
    let mut c = Config::parse();
    if let Some(value) = env::var_os("XR0_INCLUDES") {
        let path: &Path = value.as_ref();
        c.include_dirs.insert(0, path.to_path_buf());
    }

    unsafe {
        VERBOSE_MODE = c.verbose as libc::c_int;
    }
    let result = if c.strip_mode {
        strip(&c)
    } else {
        unsafe { verify(&c) }
    };
    if let Err(err) = result {
        eprintln!("{err}");
        process::exit(1);
    }
}
