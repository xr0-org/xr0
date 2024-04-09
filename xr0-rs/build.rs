use std::env;

fn main() {
    cc::Build::new()
        .include("../include")
        .include("../src/ast")
        .file("../build/lex.yy.c")
        .file("../build/gram.tab.c")
        .compile("xr0parse");
    //let out_dir = env::var("OUT_DIR").unwrap();
    //println!("cargo::rustc-link-search=native=/Users/jorendorff/src/xr0/build");
    //println!("cargo::rustc-link-lib=static=xr0parse");
    println!("cargo::rerun-if-changed=../build/lex.yy.c");
    println!("cargo::rerun-if-changed=../build/gram.tab.c");
}
