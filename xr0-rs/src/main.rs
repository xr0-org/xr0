use std::env;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};

use clap::Parser;
use libc::strcmp;

mod util;

mod ast;
mod ext;
mod math;
mod object;
mod parser;
mod props;
mod state;
mod value;

use ast::{
    ast_block_str, ast_externdecl_as_function, ast_externdecl_as_function_ptr,
    ast_externdecl_install, ast_function_absisempty, ast_function_abstract, ast_function_copy,
    ast_function_isaxiom, ast_function_isproto, ast_function_name, ast_function_verify,
    ast_functiondecl_create, ast_protostitch, ast_topological_order, Ast, AstExpr, AstExternDecl,
    AstFunction, AstType, AstVariable,
};
use ext::Externals;
use object::Object;
use props::Props;
use state::block::{Block, BlockArr};
use state::clump::Clump;
use state::heap::{Heap, VConst};
use state::location::Location;
use state::r#static::StaticMemory;
use state::stack::{Stack, Variable};
use state::state::State;
use util::{string_arr_n, string_arr_s, string_arr_str, StrBuilder, StringArr, VERBOSE_MODE};
use value::Value;

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

pub unsafe fn pass0(root: &mut Ast, ext: &mut Externals) {
    for decl in &root.decls {
        match ast_externdecl_as_function_ptr(decl) {
            None => {
                ast_externdecl_install(&**decl as *const AstExternDecl as *mut AstExternDecl, ext);
            }
            Some(f) => {
                if ast_function_isaxiom(&*f) {
                    ast_externdecl_install(
                        &**decl as *const AstExternDecl as *mut AstExternDecl,
                        ext,
                    );
                } else if ast_function_isproto(&*f) {
                    if !verifyproto(&*f, &root.decls) {
                        process::exit(1);
                    }
                    ast_externdecl_install(
                        &**decl as *const AstExternDecl as *mut AstExternDecl,
                        ext,
                    );
                } else {
                    let stitched: *mut AstFunction = ast_protostitch(f, ext);
                    ast_externdecl_install(
                        Box::into_raw(ast_functiondecl_create(ast_function_copy(&*stitched))),
                        ext,
                    );
                }
            }
        }
    }
}

pub unsafe fn pass1(root: &mut Ast, ext: *mut Externals) {
    for decl in &mut root.decls {
        if let Some(f) = ast_externdecl_as_function_ptr(decl) {
            if !ast_function_isaxiom(&*f) && !ast_function_isproto(&*f) {
                if let Err(err) = ast_function_verify(f, ext) {
                    eprintln!("{}", err.msg);
                    process::exit(1);
                }
                vprintln!(
                    "qed {}",
                    CStr::from_ptr(ast_function_name(&*f)).to_string_lossy()
                );
            }
        }
    }
}

pub unsafe fn pass_inorder(order: &mut StringArr, ext: &mut Externals) {
    let n: libc::c_int = string_arr_n(order);
    let name: *mut *mut libc::c_char = string_arr_s(order);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let f: *mut AstFunction = ext.get_func(*name.offset(i as isize));
        if !(ast_function_isaxiom(&*f) as libc::c_int != 0
            || ast_function_isproto(&*f) as libc::c_int != 0)
        {
            if let Err(err) = ast_function_verify(f, ext) {
                eprintln!("{}", err.msg);
                process::exit(1);
            }
            eprintln!(
                "qed {}",
                CStr::from_ptr(ast_function_name(&*f)).to_string_lossy()
            );
        }
        i += 1;
    }
}

unsafe fn verifyproto(proto: &AstFunction, decls: &[Box<AstExternDecl>]) -> bool {
    let mut def: Option<&AstFunction> = None;
    let mut count: libc::c_int = 0 as libc::c_int;
    let pname: *mut libc::c_char = ast_function_name(proto);
    for decl in decls {
        if let Some(d) = ast_externdecl_as_function(decl) {
            if !ast_function_isaxiom(d)
                && !ast_function_isproto(d)
                && strcmp(pname, ast_function_name(d)) == 0
            {
                def = Some(d);
                count += 1;
            }
        }
    }
    if count == 1 {
        if proto_defisvalid(proto, def.unwrap()) {
            return true;
        }
        eprintln!(
            "function `{}' prototype and definition abstracts mismatch",
            CStr::from_ptr(pname).to_string_lossy()
        );
    } else if count == 0 as libc::c_int {
        eprintln!(
            "function `{}' missing definition",
            CStr::from_ptr(pname).to_string_lossy()
        );
    } else if count > 1 as libc::c_int {
        eprintln!(
            "function `{}' has multiple definitions",
            CStr::from_ptr(pname).to_string_lossy()
        );
    }
    false
}

unsafe fn proto_defisvalid(proto: &AstFunction, def: &AstFunction) -> bool {
    let proto_abs = ast_function_abstract(proto);
    let def_abs = ast_function_abstract(def);
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
    let protoabs_only: bool = ast_function_absisempty(def) as libc::c_int != 0;
    abs_match || protoabs_only
}

unsafe fn verify(c: &Config) -> io::Result<()> {
    let source = preprocess(&c.infile, &c.include_dirs)?;
    let mut root = parser::parse_translation_unit(&c.infile, &source)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{err}")))?;

    let mut ext = Externals::new();
    pass0(&mut root, &mut ext);

    if let Some(sortfunc) = &c.sort {
        let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
        let order = ast_topological_order(sortfunc_cstr.as_ptr() as *mut libc::c_char, &mut ext);
        eprintln!(
            "{}",
            CStr::from_ptr(string_arr_str(&order)).to_string_lossy()
        );
    } else if let Some(sortfunc) = &c.verify {
        let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
        let mut order =
            ast_topological_order(sortfunc_cstr.as_ptr() as *mut libc::c_char, &mut ext);
        eprintln!(
            "{}",
            CStr::from_ptr(string_arr_str(&order)).to_string_lossy()
        );
        pass_inorder(&mut order, &mut ext);
    } else {
        pass1(&mut root, &mut ext);
    }

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
