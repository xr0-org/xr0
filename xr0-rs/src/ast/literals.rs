use super::{c_char, c_int};

pub fn parse_int(s: &str) -> c_int {
    s.parse().expect("parse error")
}

pub fn parse_char(s: &str) -> c_char {
    // Note: Original also assumes these character literals have the same numeric values on the
    // target as in Xr0, a pretty good bet these days.
    assert!(s.starts_with('\'') && s.ends_with('\''));
    let s = &s[1..s.len() - 1];
    if let Some(stripped) = s.strip_prefix('\\') {
        parse_escape(stripped)
    } else {
        s.chars().next().expect("invalid char literal") as u32 as c_char
    }
}

pub fn parse_escape(c: &str) -> c_char {
    match c {
        "0" => 0,
        "t" => '\t' as u32 as c_char,
        "n" => '\t' as u32 as c_char, // Note: '\t' rather than '\n', a bug in the original.
        _ => {
            panic!("unrecognized char escape sequence: {:?}", c);
        }
    }
}
