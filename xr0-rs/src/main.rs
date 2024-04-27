#![deny(warnings)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::vec_box)]
#![allow(clippy::box_collection)]

use std::env;
use std::ffi::CString;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};

use clap::Parser;

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
    ast_externdecl_install, ast_function_verify, ast_functiondecl_create, ast_protostitch,
    ast_topological_order, Ast, AstExpr, AstExternDecl, AstFunction, AstType, AstVariable,
};
use ext::Externals;
use object::Object;
use props::Props;
use state::location::Location;
use util::{OwningCStr, VERBOSE_MODE};
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
                if (*f).is_axiom() {
                    ast_externdecl_install(
                        &**decl as *const AstExternDecl as *mut AstExternDecl,
                        ext,
                    );
                } else if (*f).is_proto() {
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
                        Box::into_raw(ast_functiondecl_create((*stitched).copy())),
                        ext,
                    );
                }
            }
        }
    }
}

pub unsafe fn pass1(root: &mut Ast, ext: &Externals) {
    for decl in &mut root.decls {
        if let Some(f) = ast_externdecl_as_function(decl) {
            if !f.is_axiom() && !f.is_proto() {
                if let Err(err) = ast_function_verify(f, ext) {
                    eprintln!("{}", err.msg);
                    process::exit(1);
                }
                vprintln!("qed {}", f.name());
            }
        }
    }
}

pub unsafe fn pass_inorder(order: &[OwningCStr], ext: &Externals) {
    for name in order {
        let f = ext.get_func(name.as_str()).unwrap();
        if !(f.is_axiom() || f.is_proto()) {
            if let Err(err) = ast_function_verify(f, ext) {
                eprintln!("{}", err.msg);
                process::exit(1);
            }
            eprintln!("qed {}", (*f).name());
        }
    }
}

unsafe fn verifyproto(proto: &AstFunction, decls: &[Box<AstExternDecl>]) -> bool {
    let mut def: Option<&AstFunction> = None;
    let mut count: libc::c_int = 0 as libc::c_int;
    let pname = proto.name();
    for decl in decls {
        if let Some(d) = ast_externdecl_as_function(decl) {
            if !d.is_axiom() && !d.is_proto() && pname == d.name() {
                def = Some(d);
                count += 1;
            }
        }
    }
    if count == 1 {
        if proto_defisvalid(proto, def.unwrap()) {
            return true;
        }
        eprintln!("function `{pname}' prototype and definition abstracts mismatch");
    } else if count == 0 {
        eprintln!("function `{pname}' missing definition");
    } else if count > 1 {
        eprintln!("function `{pname}' has multiple definitions");
    }
    false
}

unsafe fn proto_defisvalid(proto: &AstFunction, def: &AstFunction) -> bool {
    let proto_abs = proto.abstract_block();
    let def_abs = def.abstract_block();
    let abs_match = ast_block_str(proto_abs, "") == ast_block_str(def_abs, "");
    let protoabs_only = def.abs_is_empty();
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
        let order = ast_topological_order(&sortfunc_cstr, &mut ext);
        let strs: Vec<&str> = order.iter().map(|f| f.as_str()).collect();
        eprintln!("{}", strs.join(", "));
    } else if let Some(sortfunc) = &c.verify {
        let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
        let order = ast_topological_order(&sortfunc_cstr, &mut ext);
        let strs: Vec<&str> = order.iter().map(|f| f.as_str()).collect();
        eprintln!("{}", strs.join(", "));
        pass_inorder(&order, &ext);
    } else {
        pass1(&mut root, &ext);
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
