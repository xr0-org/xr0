#![deny(warnings)]
#![allow(clippy::vec_box)]
#![allow(clippy::box_collection)]

use std::env;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};
use std::sync::atomic::Ordering;
use std::sync::Arc;

use clap::Parser;

use xr0::ast::{
    ast_function_verify, ast_protostitch, ast_topological_order, Ast, AstExternDecl, AstFunction,
};
use xr0::util::VERBOSE_MODE;
use xr0::{parser, vprintln, Externals};

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
fn pass0(root: Box<Ast>, ext: &mut Externals) {
    // Rust note: This clone is not in the original. The cost could be reduced by strategically
    // changing Boxes to Arcs in the AST. The cost could also probably be eliminated entirely using
    // SemiBox and giving Externals a lifetime parameter, but that's a bit much.

    for mut decl in root.decls.clone() {
        match decl.as_function_mut() {
            None => {
                ext.add(decl);
            }
            Some(f) => {
                if f.is_axiom() {
                    ext.add(decl);
                } else if f.is_proto() {
                    if !verifyproto(f, &root.decls) {
                        process::exit(1);
                    }
                    ext.add(decl);
                } else {
                    ast_protostitch(f, ext);
                    ext.add(AstExternDecl::new_function(f.copy()));
                }
            }
        }
    }
}

fn pass1(order: &[String], ext: &Externals) {
    for name in order {
        let f = ext.get_func(name.as_str()).unwrap();
        if !f.is_axiom() && !f.is_proto() {
            if let Err(err) = ast_function_verify(f, ext) {
                eprintln!("{err}");
                process::exit(1);
            }
            vprintln!("qed {}", f.name());
        }
    }
}

fn verifyproto(proto: &AstFunction, decls: &[Box<AstExternDecl>]) -> bool {
    let mut def: Option<&AstFunction> = None;
    let mut count = 0usize;
    let pname = proto.name();
    for decl in decls {
        if let Some(d) = decl.as_function() {
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
    let proto_abs = &proto.abstract_;
    let def_abs = &def.abstract_;
    let abs_match = proto_abs.str("") == def_abs.str("");
    let protoabs_only = def.abs_is_empty();
    abs_match || protoabs_only
}

fn verify(c: &Config) -> io::Result<()> {
    let source = preprocess(&c.infile, &c.include_dirs)?;
    let root = parser::parse_translation_unit(&c.infile, &source)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{err}")))?;

    let mut ext = Externals::new();
    pass0(root, &mut ext);

    let ext = Arc::new(ext);
    if let Some(sortfunc) = &c.sort {
        let order = ast_topological_order(sortfunc, &ext);
        let strs: Vec<&str> = order.iter().map(|f| f.as_str()).collect();
        eprintln!("{}", strs.join(", "));
    } else if let Some(sortfunc) = &c.verify {
        let order = ast_topological_order(sortfunc, &ext);
        let strs: Vec<&str> = order.iter().map(|f| f.as_str()).collect();
        eprintln!("{}", strs.join(", "));
        pass1(&order, &ext);
    } else {
        pass1(ext.function_names(), &ext);
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
    let result = if c.strip_mode { strip(&c) } else { verify(&c) };
    if let Err(err) = result {
        eprintln!("{err}");
        process::exit(1);
    }
}
