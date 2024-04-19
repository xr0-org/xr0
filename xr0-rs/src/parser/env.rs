use std::cell::RefCell;
use std::collections::HashSet;
use std::ffi::CStr;
use std::path::{Path, PathBuf};

use libc::{free, malloc};

use crate::util::{self, dynamic_str, strbuilder_build, strbuilder_create, StrBuilder};
use crate::{cstr, strbuilder_write};

pub struct Env {
    source: String,
    pub reserved: HashSet<&'static str>,
    pub typenames: RefCell<HashSet<Vec<u8>>>,
    // these triples are (region start offset, filename, starting line)
    pub regions: Vec<(usize, PathBuf, usize)>,
}

static RESERVED: &[&str] = &[
    "auto", "axiom", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "int", "lemma", "long", "register",
    "return", "short", "signed", "sizeof", "some", "static", "sfunc", "struct", "switch",
    "typedef", "union", "unsigned", "void", "volatile", "while",
];

impl Env {
    pub fn new(filename: &Path, source: &str) -> Self {
        // Scan the whole input for line markers. This is necessary since the peg parser rewinds,
        // so interleaving parser and lexer actions doesn't make sense.
        let mut regions = vec![(0, filename.to_path_buf(), 1)];
        let mut pos = 0;
        for line in source.split_inclusive('\n') {
            pos += line.len();
            let line = line.trim_start();
            if line.starts_with('#') {
                let (file, line_num) = parse_linemarker(&line[1..]);
                regions.push((pos, file, line_num));
            }
        }

        Env {
            source: source.to_string(),
            reserved: RESERVED.iter().copied().collect(),
            typenames: RefCell::new(HashSet::new()),
            regions,
        }
    }

    pub fn file_line_column(&self, offset: usize) -> (&Path, usize, usize) {
        assert!(offset <= self.source.len(), "offset out of bounds");
        let i = match self
            .regions
            .binary_search_by_key(&offset, |region| region.0)
        {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let (region_start_offset, ref region_filename, mut line) = self.regions[i];

        let mut column = 0;
        for c in self.source[region_start_offset..offset].chars() {
            match c {
                '\n' => {
                    line += 1;
                    column = 0;
                }
                '\t' => {
                    column += 8 - column % 8;
                }
                _ => {
                    column += 1;
                }
            }
        }
        (region_filename, line, column)
    }

    pub unsafe fn lexloc(&self, p: usize) -> *mut LexemeMarker {
        let (file, line, column) = self.file_line_column(p);
        let filename_cstr = util::to_c_str(file.as_os_str().as_encoded_bytes());
        lexememarker_create(line as libc::c_int, column as libc::c_int, filename_cstr, 0)
    }

    pub unsafe fn add_typename(&self, name: *const libc::c_char) {
        let name = CStr::from_ptr(name);
        let mut typenames = self.typenames.borrow_mut();
        typenames.insert(name.to_bytes().to_vec());
    }

    pub unsafe fn is_typename(&self, name: *const libc::c_char) -> bool {
        let name = CStr::from_ptr(name);
        let typenames = self.typenames.borrow();
        typenames.contains(name.to_bytes())
    }
}

fn parse_linemarker(line: &str) -> (PathBuf, usize) {
    let mut chars = line.chars();
    let mut c = chars.next().unwrap_or('\0');
    if !c.is_whitespace() {
        eprintln!("expected space before line number");
        std::process::exit(1);
    }
    while c.is_whitespace() {
        c = chars.next().unwrap_or('\0');
    }
    if !c.is_digit(10) {
        eprintln!("expected line number in line marker");
        std::process::exit(1);
    }
    let mut linenum = 0;
    while c.is_digit(10) {
        linenum *= 10;
        linenum += (c as u32 - '0' as u32) as usize;
        c = chars.next().unwrap_or('\0');
    }
    if !c.is_whitespace() {
        eprintln!("expected space before file name");
        std::process::exit(1);
    }
    while c.is_whitespace() {
        c = chars.next().unwrap_or('\0');
    }
    if c != '"' {
        eprintln!("file name must begin with opening quote");
        std::process::exit(1);
    }
    let s = chars.as_str();
    loop {
        c = chars.next().unwrap_or('"');
        if c == '"' {
            break;
        }
    }
    let n = s.len() - chars.as_str().len() - 1; // -1 for closing quote
    let name = &s[..n];
    let mut flags: libc::c_int = 0 as libc::c_int;
    c = chars.next().unwrap_or('\0');
    if c.is_whitespace() {
        while c != '\0' {
            if c.is_digit(10) {
                match c {
                    '1' => {
                        flags |= LM_FLAG_NEW_FILE as libc::c_int;
                    }
                    '2' => {
                        flags |= LM_FLAG_RESUME_FILE as libc::c_int;
                    }
                    '3' => {
                        flags |= LM_FLAG_SYS_HEADER as libc::c_int;
                    }
                    '4' => {
                        flags |= LM_FLAG_IMPLICIT_EXTERN as libc::c_int;
                    }
                    _ => {
                        eprintln!("invalid flag {:?}", c);
                        std::process::exit(1);
                    }
                }
            }
            c = chars.next().unwrap_or('\0');
        }
    }
    let _ = flags;

    (PathBuf::from(name), linenum)
}

#[derive(Copy, Clone)]
pub struct LexemeMarker {
    pub linenum: libc::c_int,
    pub column: libc::c_int,
    pub filename: *mut libc::c_char,
    pub flags: LineMarkerFlag,
}
pub type LineMarkerFlag = libc::c_uint;
pub const LM_FLAG_IMPLICIT_EXTERN: LineMarkerFlag = 8;
pub const LM_FLAG_SYS_HEADER: LineMarkerFlag = 4;
pub const LM_FLAG_RESUME_FILE: LineMarkerFlag = 2;
pub const LM_FLAG_NEW_FILE: LineMarkerFlag = 1;

pub unsafe fn lexememarker_create(
    linenum: libc::c_int,
    column: libc::c_int,
    filename: *mut libc::c_char,
    flags: LineMarkerFlag,
) -> *mut LexemeMarker {
    let loc = malloc(::core::mem::size_of::<LexemeMarker>()) as *mut LexemeMarker;
    (*loc).linenum = linenum;
    (*loc).column = column;
    (*loc).filename = filename;
    (*loc).flags = flags;
    return loc;
}

pub unsafe fn lexememarker_copy(loc: *mut LexemeMarker) -> *mut LexemeMarker {
    return lexememarker_create(
        (*loc).linenum,
        (*loc).column,
        dynamic_str((*loc).filename),
        (*loc).flags,
    );
}

pub unsafe fn lexememarker_destroy(loc: *mut LexemeMarker) {
    free((*loc).filename as *mut libc::c_void);
    free(loc as *mut libc::c_void);
}

pub unsafe fn lexememarker_str(loc: &LexemeMarker) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_write!(b, "{}:{}:{}", cstr!(loc.filename), loc.linenum, loc.column);
    return strbuilder_build(b);
}
