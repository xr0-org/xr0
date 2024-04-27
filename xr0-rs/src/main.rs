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
use std::sync::Arc;
use std::sync::atomic::Ordering;

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
    ast_block_str, ast_externdecl_as_function, ast_externdecl_as_function_mut,
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
struct Config {
    #[arg(value_name = "FILE")]
    infile: PathBuf,

    #[arg(short, default_value = "0.c")]
    outfile: PathBuf,

    #[arg(short = 'I', action = clap::ArgAction::Append)]
    include_dirs: Vec<PathBuf>,

    #[arg(short, long)]
    verbose: bool,

    /// Function to evaluate dependencies for.
    #[arg(short = 't', group = "sortmode")]
    sort: Option<String>,

    #[arg(short = 'x', group = "sortmode")]
    verify: Option<String>,

    #[arg(short = 's', long = "strip")]
    strip_mode: bool,
}

fn preprocess(infile: &Path, include_dirs: &[PathBuf]) -> io::Result<String> {
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

#[allow(clippy::boxed_local)]
unsafe fn pass0(root: Box<Ast>, ext: &mut Externals) {
    // Note: This clone is not in the original. The cost could be reduced by strategically changing
    // Boxes to Arcs in the AST. The cost could also probably be eliminated entirely using SemiBox
    // and giving Externals a lifetime parameter, but that's a bit much.

    for mut decl in root.decls.clone() {
        match ast_externdecl_as_function_mut(&mut decl) {
            None => {
                ast_externdecl_install(decl, ext);
            }
            Some(f) => {
                if f.is_axiom() {
                    ast_externdecl_install(decl, ext);
                } else if f.is_proto() {
                    if !verifyproto(f, &root.decls) {
                        process::exit(1);
                    }
                    ast_externdecl_install(decl, ext);
                } else {
                    ast_protostitch(f, ext);
                    ast_externdecl_install(ast_functiondecl_create(f.copy()), ext);
                }
            }
        }
    }
}

unsafe fn pass1(order: &[OwningCStr], ext: &Arc<Externals>, print: bool) {
    for name in order {
        let f = ext.get_func(name.as_str()).unwrap();
        if !f.is_axiom() && !f.is_proto() {
            if let Err(err) = ast_function_verify(f, Arc::clone(ext)) {
                eprintln!("{}", err.msg);
                process::exit(1);
            }
            if print {
                eprintln!("qed {}", f.name());
            } else {
                vprintln!("qed {}", f.name());
            }
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

fn proto_defisvalid(proto: &AstFunction, def: &AstFunction) -> bool {
    let proto_abs = proto.abstract_block();
    let def_abs = def.abstract_block();
    let abs_match = ast_block_str(proto_abs, "") == ast_block_str(def_abs, "");
    let protoabs_only = def.abs_is_empty();
    abs_match || protoabs_only
}

unsafe fn verify(c: &Config) -> io::Result<()> {
    let source = preprocess(&c.infile, &c.include_dirs)?;
    let root = parser::parse_translation_unit(&c.infile, &source)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{err}")))?;

    let mut ext = Externals::new();
    pass0(root, &mut ext);

    let ext = Arc::new(ext);
    if let Some(sortfunc) = &c.sort {
        let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
        let order = ast_topological_order(&sortfunc_cstr, &ext);
        let strs: Vec<&str> = order.iter().map(|f| f.as_str()).collect();
        eprintln!("{}", strs.join(", "));
    } else if let Some(sortfunc) = &c.verify {
        let sortfunc_cstr = CString::new(sortfunc.clone()).unwrap();
        let order = ast_topological_order(&sortfunc_cstr, & ext);
        let strs: Vec<&str> = order.iter().map(|f| f.as_str()).collect();
        eprintln!("{}", strs.join(", "));
        pass1(&order, &ext, true);
    } else {
        pass1(ext.function_names(), &ext, false);
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

fn main() {
    let mut c = Config::parse();
    if let Some(value) = env::var_os("XR0_INCLUDES") {
        let path: &Path = value.as_ref();
        c.include_dirs.insert(0, path.to_path_buf());
    }

    VERBOSE_MODE.store(c.verbose, Ordering::Relaxed);
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
