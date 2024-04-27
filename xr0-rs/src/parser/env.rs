use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};

pub struct Env {
    source: String,
    pub reserved: HashSet<&'static str>,
    pub typenames: RefCell<HashSet<String>>,
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
            if let Some(rest) = line.strip_prefix('#') {
                let (file, line_num) = parse_linemarker(rest);
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

    pub fn lexloc(&self, p: usize) -> Box<LexemeMarker> {
        let (file, line, column) = self.file_line_column(p);
        let filename = file.display().to_string();
        Box::new(LexemeMarker {
            linenum: line as libc::c_int,
            column: column as libc::c_int,
            filename,
            flags: 0,
        })
    }

    pub fn add_typename(&self, name: &str) {
        let mut typenames = self.typenames.borrow_mut();
        typenames.insert(name.to_string());
    }

    pub fn is_typename(&self, name: &str) -> bool {
        let typenames = self.typenames.borrow();
        typenames.contains(name)
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
    if !c.is_ascii_digit() {
        eprintln!("expected line number in line marker");
        std::process::exit(1);
    }
    let mut linenum = 0;
    while c.is_ascii_digit() {
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
            if c.is_ascii_digit() {
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

#[derive(Clone)]
pub struct LexemeMarker {
    pub linenum: libc::c_int,
    pub column: libc::c_int,
    pub filename: String,
    pub flags: LineMarkerFlag,
}

pub type LineMarkerFlag = libc::c_uint;
pub const LM_FLAG_IMPLICIT_EXTERN: LineMarkerFlag = 8;
pub const LM_FLAG_SYS_HEADER: LineMarkerFlag = 4;
pub const LM_FLAG_RESUME_FILE: LineMarkerFlag = 2;
pub const LM_FLAG_NEW_FILE: LineMarkerFlag = 1;

impl Display for LexemeMarker {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.linenum, self.column)
    }
}
