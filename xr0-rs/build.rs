fn main() {
    cc::Build::new()
        .include("../include")
        .include("../src/ast")
        .file("../build/lex.yy.c")
        .file("../build/gram.tab.c")
        .compile("xr0parse");
    println!("cargo::rerun-if-changed=../build/lex.yy.c");
    println!("cargo::rerun-if-changed=../build/gram.tab.c");
}
