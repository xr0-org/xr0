#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_variables
)]
#![feature(c_variadic, extern_types, linkage)]

mod ast;
mod c_util;
mod ext;
mod math;
mod object;
mod parser;
mod props;
mod state;
mod util;
mod value;

use crate::ast::{
    ast_block, ast_externdecl, ast_function, ast_type, ast_variable, ast_variable_arr, Ast, AstExpr,
};
use crate::ext::Externals;
use crate::math::math_expr;
use crate::object::{object_arr, Object};
use crate::props::Props;
use crate::state::block::{block_arr, Block};
use crate::state::clump::Clump;
use crate::state::heap::{vconst, Heap};
use crate::state::location::Location;
use crate::state::r#static::static_memory;
use crate::state::stack::{Stack, Variable};
use crate::state::State;
use crate::util::StrBuilder;
type strbuilder = StrBuilder;
use crate::value::Value;

// NOTE: libc::isspace may be slower than the inlined definition i deleted
use libc::isspace;
use libc::{
    exit, fgetc, fgets, fprintf, fputs, free, fseek, getenv, malloc, pclose, popen, rewind,
    snprintf, strcmp, strlen, tmpfile,
};

use ast::{
    ast_block_str, ast_destroy, ast_externdecl_as_function, ast_externdecl_install,
    ast_externdecl_isfunction, ast_function_absisempty, ast_function_abstract, ast_function_copy,
    ast_function_isaxiom, ast_function_isproto, ast_function_name, ast_function_verify,
    ast_functiondecl_create, ast_protostitch, ast_topological_order,
};
use c_util::{__assert_rtn, __stderrp, getopt, optarg, optind};
use ext::{externals_create, externals_destroy, externals_getfunc};
use parser::gram::yyparse;
use parser::lexer::{lex_begin, lex_finish, yyin, yylex_destroy};
use util::{
    dynamic_str, error, strbuilder_build, strbuilder_create, strbuilder_printf, string_arr,
    string_arr_append, string_arr_create, string_arr_n, string_arr_s, string_arr_str, v_printf,
    VERBOSE_MODE,
};

pub type __uint32_t = libc::c_uint;
pub type __int64_t = libc::c_longlong;

pub type execmode = libc::c_uint;
pub const EXECMODE_STRIP: execmode = 1;
pub const EXECMODE_VERIFY: execmode = 0;
pub type sortmode = libc::c_uint;
pub const SORTMODE_VERIFY: sortmode = 2;
pub const SORTMODE_SORT: sortmode = 1;
pub const SORTMODE_NONE: sortmode = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct config {
    pub infile: *mut libc::c_char,
    pub outfile: *mut libc::c_char,
    pub includedirs: *mut string_arr,
    pub verbose: bool,
    pub mode: execmode,
    pub sortfunc: *mut libc::c_char,
    pub sortmode: sortmode,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sortconfig {
    pub mode: sortmode,
    pub sortfunc: *mut libc::c_char,
}
#[inline]
unsafe fn isascii(mut _c: libc::c_int) -> libc::c_int {
    return (_c & !(0x7f as libc::c_int) == 0 as libc::c_int) as libc::c_int;
}

#[no_mangle]
pub unsafe fn parse_config(mut argc: libc::c_int, mut argv: *mut *mut libc::c_char) -> config {
    let mut mode: execmode = EXECMODE_VERIFY;
    let mut verbose: bool = 0 as libc::c_int != 0;
    let mut sortconf: sortconfig = sortconfig_create(
        SORTMODE_NONE,
        b"\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    let mut includedirs: *mut string_arr = default_includes();
    let mut outfile: *mut libc::c_char =
        b"0.c\0" as *const u8 as *const libc::c_char as *mut libc::c_char;
    let mut opt: libc::c_int = 0;
    loop {
        opt = getopt(
            argc,
            argv as *const *mut libc::c_char,
            b"vso:t:x:I:\0" as *const u8 as *const libc::c_char,
        );
        if !(opt != -(1 as libc::c_int)) {
            break;
        }
        match opt {
            73 => {
                string_arr_append(includedirs, dynamic_str(optarg));
            }
            111 => {
                outfile = optarg;
            }
            118 => {
                verbose = 1 as libc::c_int != 0;
            }
            115 => {
                mode = EXECMODE_STRIP;
            }
            116 => {
                sortconf = sortconfig_create(SORTMODE_SORT, optarg);
            }
            120 => {
                sortconf = sortconfig_create(SORTMODE_VERIFY, optarg);
            }
            _ => {
                fprintf(
                    __stderrp,
                    b"Usage: %s [-I libx] input_file\n\0" as *const u8 as *const libc::c_char,
                    *argv.offset(0 as libc::c_int as isize),
                );
                exit(1 as libc::c_int);
            }
        }
    }
    if optind >= argc {
        fprintf(
            __stderrp,
            b"%s\n\0" as *const u8 as *const libc::c_char,
            b"must provide input as string\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    return {
        let mut init = config {
            infile: *argv.offset(optind as isize),
            outfile: outfile,
            includedirs: includedirs,
            verbose: verbose,
            mode: mode,
            sortfunc: sortconf.sortfunc,
            sortmode: sortconf.mode,
        };
        init
    };
}
unsafe fn sortconfig_create(mut mode: sortmode, mut sortfunc: *mut libc::c_char) -> sortconfig {
    match mode as libc::c_uint {
        0 => {
            return {
                let mut init = sortconfig {
                    mode: mode,
                    sortfunc: sortfunc,
                };
                init
            };
        }
        1 | 2 => {
            if sortfunc.is_null() {
                fprintf(
                    __stderrp,
                    b"%s\n\0" as *const u8 as *const libc::c_char,
                    b"supply function to `-t' flag to evaluate dependencies for\0" as *const u8
                        as *const libc::c_char,
                );
                exit(1 as libc::c_int);
            }
            return {
                let mut init = sortconfig {
                    mode: mode,
                    sortfunc: sortfunc,
                };
                init
            };
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(
                        b"sortconfig_create\0",
                    ))
                    .as_ptr(),
                    b"main.c\0" as *const u8 as *const libc::c_char,
                    123 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
unsafe fn default_includes() -> *mut string_arr {
    let mut dirs: *mut string_arr = string_arr_create();
    let mut env: *mut libc::c_char = getenv(b"XR0_INCLUDES\0" as *const u8 as *const libc::c_char);
    if !env.is_null() {
        string_arr_append(dirs, env);
    }
    return dirs;
}
#[no_mangle]
pub unsafe fn genincludes(mut includedirs: *mut string_arr) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut s: *mut *mut libc::c_char = string_arr_s(includedirs);
    let mut n: libc::c_int = string_arr_n(includedirs);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        strbuilder_printf(
            b,
            b" -I %s\0" as *const u8 as *const libc::c_char,
            *s.offset(i as isize),
        );
        i += 1;
    }
    return strbuilder_build(b);
}

#[no_mangle]
pub unsafe fn preprocesscmd_fmt(
    mut includedirs: *mut string_arr,
    mut infile: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut includes: *mut libc::c_char = genincludes(includedirs);
    let mut len: libc::c_int =
        (strlen(b"cc %s -nostdinc -E -xc %s\0" as *const u8 as *const libc::c_char))
            .wrapping_sub(4)
            .wrapping_add(strlen(includes))
            .wrapping_add(strlen(infile))
            .wrapping_add(1) as libc::c_int;
    let mut s: *mut libc::c_char =
        malloc((::core::mem::size_of::<libc::c_char>()).wrapping_mul(len as usize))
            as *mut libc::c_char;
    snprintf(
        s,
        len as usize,
        b"cc %s -nostdinc -E -xc %s\0" as *const u8 as *const libc::c_char,
        includes,
        infile,
    );
    return s;
}

#[no_mangle]
pub unsafe fn open_preprocessor(
    mut infile: *mut libc::c_char,
    mut includedirs: *mut string_arr,
) -> *mut libc::FILE {
    let mut cmd: *mut libc::c_char = preprocesscmd_fmt(includedirs, infile);
    let mut pipe: *mut libc::FILE = popen(cmd, b"r\0" as *const u8 as *const libc::c_char);
    free(cmd as *mut libc::c_void);
    return pipe;
}
#[no_mangle]
pub unsafe fn preprocess(
    mut infile: *mut libc::c_char,
    mut includedirs: *mut string_arr,
) -> *mut libc::FILE {
    let mut pipe: *mut libc::FILE = open_preprocessor(infile, includedirs);
    if pipe.is_null() {
        fprintf(
            __stderrp,
            b"command error\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    let mut tmp: *mut libc::FILE = tmpfile();
    if tmp.is_null() {
        fprintf(
            __stderrp,
            b"cannot create temp file\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    let mut buf: [libc::c_char; 1024] = [0; 1024];
    while !(fgets(
        buf.as_mut_ptr(),
        ::core::mem::size_of::<[libc::c_char; 1024]>() as libc::c_ulong as libc::c_int,
        pipe,
    ))
    .is_null()
    {
        fputs(buf.as_mut_ptr(), tmp);
    }
    pclose(pipe);
    rewind(tmp);
    return tmp;
}
#[no_mangle]
pub static mut root: *mut Ast = 0 as *const Ast as *mut Ast;
#[no_mangle]
pub unsafe fn pass0(mut root_0: *mut Ast, mut ext: *mut Externals) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*root_0).n {
        let mut decl: *mut ast_externdecl = *((*root_0).decl).offset(i as isize);
        if !ast_externdecl_isfunction(decl) {
            ast_externdecl_install(decl, ext);
        } else {
            let mut f: *mut ast_function = ast_externdecl_as_function(decl);
            if ast_function_isaxiom(f) {
                ast_externdecl_install(decl, ext);
            } else if ast_function_isproto(f) {
                if !verifyproto(f, (*root_0).n, (*root_0).decl) {
                    exit(1 as libc::c_int);
                }
                ast_externdecl_install(decl, ext);
            } else {
                let mut stitched: *mut ast_function = ast_protostitch(f, ext);
                ast_externdecl_install(ast_functiondecl_create(ast_function_copy(stitched)), ext);
            }
        }
        i += 1;
    }
}
#[no_mangle]
pub unsafe fn pass1(mut root_0: *mut Ast, mut ext: *mut Externals) {
    let mut err: *mut error = 0 as *mut error;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*root_0).n {
        let mut decl: *mut ast_externdecl = *((*root_0).decl).offset(i as isize);
        if ast_externdecl_isfunction(decl) {
            let mut f: *mut ast_function = ast_externdecl_as_function(decl);
            if !(ast_function_isaxiom(f) as libc::c_int != 0
                || ast_function_isproto(f) as libc::c_int != 0)
            {
                if (ast_function_abstract(f)).is_null() as libc::c_int as libc::c_long != 0 {
                    __assert_rtn(
                        (*::core::mem::transmute::<&[u8; 6], &[libc::c_char; 6]>(b"pass1\0"))
                            .as_ptr(),
                        b"main.c\0" as *const u8 as *const libc::c_char,
                        243 as libc::c_int,
                        b"ast_function_abstract(f)\0" as *const u8 as *const libc::c_char,
                    );
                } else {
                };
                err = ast_function_verify(f, ext);
                if !err.is_null() {
                    fprintf(
                        __stderrp,
                        b"%s\n\0" as *const u8 as *const libc::c_char,
                        (*err).msg,
                    );
                    exit(1 as libc::c_int);
                }
                v_printf(
                    b"qed %s\n\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
                    ast_function_name(f),
                );
            }
        }
        i += 1;
    }
}
#[no_mangle]
pub unsafe fn pass_inorder(mut order: *mut string_arr, mut ext: *mut Externals) {
    let mut err: *mut error = 0 as *mut error;
    let mut n: libc::c_int = string_arr_n(order);
    let mut name: *mut *mut libc::c_char = string_arr_s(order);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut f: *mut ast_function = externals_getfunc(ext, *name.offset(i as isize));
        if !(ast_function_isaxiom(f) as libc::c_int != 0
            || ast_function_isproto(f) as libc::c_int != 0)
        {
            if (ast_function_abstract(f)).is_null() as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"pass_inorder\0"))
                        .as_ptr(),
                    b"main.c\0" as *const u8 as *const libc::c_char,
                    265 as libc::c_int,
                    b"ast_function_abstract(f)\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
            err = ast_function_verify(f, ext);
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
    mut proto: *mut ast_function,
    mut n: libc::c_int,
    mut decl: *mut *mut ast_externdecl,
) -> bool {
    let mut def: *mut ast_function = 0 as *mut ast_function;
    let mut count: libc::c_int = 0 as libc::c_int;
    let mut pname: *mut libc::c_char = ast_function_name(proto);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut decl_0: *mut ast_externdecl = *((*root).decl).offset(i as isize);
        if ast_externdecl_isfunction(decl_0) {
            let mut d: *mut ast_function = ast_externdecl_as_function(decl_0);
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
unsafe fn proto_defisvalid(mut proto: *mut ast_function, mut def: *mut ast_function) -> bool {
    let mut proto_abs: *mut ast_block = ast_function_abstract(proto);
    let mut def_abs: *mut ast_block = ast_function_abstract(def);
    let mut abs_match: bool = strcmp(
        ast_block_str(
            proto_abs,
            b"\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ),
        ast_block_str(
            def_abs,
            b"\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ),
    ) == 0 as libc::c_int;
    let mut protoabs_only: bool =
        !proto_abs.is_null() && ast_function_absisempty(def) as libc::c_int != 0;
    if abs_match as libc::c_int != 0 || protoabs_only as libc::c_int != 0 {
        return 1 as libc::c_int != 0;
    }
    return 0 as libc::c_int != 0;
}
unsafe fn main_0(mut argc: libc::c_int, mut argv: *mut *mut libc::c_char) -> libc::c_int {
    let mut c: config = parse_config(argc, argv);
    VERBOSE_MODE = c.verbose as libc::c_int;
    match c.mode as libc::c_uint {
        0 => return verify(&mut c),
        1 => return strip(&mut c),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 5], &[libc::c_char; 5]>(b"main\0")).as_ptr(),
                    b"main.c\0" as *const u8 as *const libc::c_char,
                    351 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    return 0;
}
unsafe fn verify(mut c: *mut config) -> libc::c_int {
    yyin = preprocess((*c).infile, (*c).includedirs);
    lex_begin();
    yyparse();
    yylex_destroy();
    lex_finish();
    let mut ext: *mut Externals = externals_create();
    pass0(root, ext);
    let mut order: *mut string_arr = 0 as *mut string_arr;
    match (*c).sortmode as libc::c_uint {
        0 => {
            pass1(root, ext);
        }
        1 => {
            order = ast_topological_order((*c).sortfunc, ext);
            fprintf(
                __stderrp,
                b"%s\n\0" as *const u8 as *const libc::c_char,
                string_arr_str(order),
            );
        }
        2 => {
            order = ast_topological_order((*c).sortfunc, ext);
            fprintf(
                __stderrp,
                b"%s\n\0" as *const u8 as *const libc::c_char,
                string_arr_str(order),
            );
            pass_inorder(order, ext);
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 7], &[libc::c_char; 7]>(b"verify\0")).as_ptr(),
                    b"main.c\0" as *const u8 as *const libc::c_char,
                    392 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    externals_destroy(ext);
    ast_destroy(root);
    return 0 as libc::c_int;
}
unsafe fn strip(mut config: *mut config) -> libc::c_int {
    let mut in_0: *mut libc::FILE = libc::fopen(
        (*config).infile,
        b"rb\0" as *const u8 as *const libc::c_char,
    );
    let mut out: *mut libc::FILE = libc::fopen(
        (*config).outfile,
        b"w\0" as *const u8 as *const libc::c_char,
    );
    let mut c: libc::c_char = 0;
    loop {
        c = fgetc(in_0) as libc::c_char;
        if !(c as libc::c_int != -(1 as libc::c_int)) {
            break;
        }
        if isvblock(c, in_0) {
            skipvblock(in_0);
        } else {
            libc::fputc(c as libc::c_int, out);
        }
    }
    libc::fclose(in_0);
    libc::fclose(out);
    return 0 as libc::c_int;
}
#[no_mangle]
pub unsafe fn isvblock(mut c: libc::c_char, mut f: *mut libc::FILE) -> bool {
    if c as libc::c_int != '~' as i32 {
        return 0 as libc::c_int != 0;
    }
    let mut pos: libc::c_long = libc::ftell(f);
    c = fgetc(f) as libc::c_char;
    while isspace(c as libc::c_int) != 0 {
        c = fgetc(f) as libc::c_char;
    }
    match c as libc::c_int {
        91 => return 1 as libc::c_int != 0,
        -1 => {
            fseek(f, -(1 as libc::c_int) as libc::c_long, 1 as libc::c_int);
            return 0 as libc::c_int != 0;
        }
        _ => {
            fseek(f, pos, 0 as libc::c_int);
            return 0 as libc::c_int != 0;
        }
    };
}

#[no_mangle]
pub unsafe fn skipvblock(mut f: *mut libc::FILE) {
    let mut c: libc::c_char = 0;
    let mut count: libc::c_int = 0 as libc::c_int;
    loop {
        c = fgetc(f) as libc::c_char;
        if !(c as libc::c_int != ']' as i32 || count != 0) {
            break;
        }
        match c as libc::c_int {
            91 => {
                count += 1;
            }
            93 => {
                count -= 1;
            }
            _ => {}
        }
    }
}

pub fn main() {
    let mut args: Vec<*mut libc::c_char> = Vec::new();
    for arg in ::std::env::args() {
        args.push(
            (::std::ffi::CString::new(arg))
                .expect("Failed to convert argument into CString.")
                .into_raw(),
        );
    }
    args.push(::core::ptr::null_mut());
    unsafe {
        ::std::process::exit(main_0(
            (args.len() - 1) as libc::c_int,
            args.as_mut_ptr() as *mut *mut libc::c_char,
        ) as i32)
    }
}
