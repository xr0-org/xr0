use std::fmt::{self, Display, Formatter};

use super::{c_int, str_write, AstVariable, Externals, State, Value};

// Note: In the original, `ast_type_copy` would not always copy modifiers.
#[derive(Clone)]
pub struct AstType {
    pub modifiers: AstTypeModifiers,
    pub base: AstTypeBase,
}

pub type AstTypeModifiers = u32;
pub const MOD_TYPEDEF: AstTypeModifiers = 1;
pub const MOD_EXTERN: AstTypeModifiers = 2;
pub const MOD_STATIC: AstTypeModifiers = 4;
pub const MOD_AUTO: AstTypeModifiers = 8;
pub const MOD_REGISTER: AstTypeModifiers = 16;
pub const MOD_CONST: AstTypeModifiers = 32;
pub const MOD_VOLATILE: AstTypeModifiers = 64;

#[derive(Clone)]
pub enum AstTypeBase {
    Void,
    Char,
    #[allow(dead_code)]
    Short,
    Int,
    #[allow(dead_code)]
    Long,
    #[allow(dead_code)]
    Float,
    #[allow(dead_code)]
    Double,
    #[allow(dead_code)]
    Signed,
    #[allow(dead_code)]
    Unsigned,
    Pointer(Box<AstType>),
    #[allow(dead_code)]
    Array(AstArrayType),
    Struct(AstStructType),
    #[allow(dead_code)]
    Union(AstStructType),
    #[allow(dead_code)]
    Enum,
    UserDefined(String),
}

#[derive(Clone)]
pub struct AstArrayType {
    pub type_: Box<AstType>,
    pub length: c_int,
}

#[derive(Clone)]
pub struct AstStructType {
    pub tag: Option<String>,
    pub members: Option<Box<Vec<Box<AstVariable>>>>,
}

//=ast_type_str
//=ast_type_str_build_ptr
//=ast_type_str_build_arr
impl Display for AstType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", mod_str(self.modifiers))?;
        match &self.base {
            AstTypeBase::Pointer(ptr_type) => {
                let space = !matches!(ptr_type.base, AstTypeBase::Pointer(_));
                write!(f, "{ptr_type}{}*", if space { " " } else { "" })
            }
            AstTypeBase::Array(AstArrayType { type_, length }) => write!(f, "{type_}[{length}]"),
            AstTypeBase::Struct(s) => write!(f, "{s}"),
            AstTypeBase::UserDefined(name) => write!(f, "{name}"),
            AstTypeBase::Void => write!(f, "void"),
            AstTypeBase::Char => write!(f, "char"),
            AstTypeBase::Short => write!(f, "short"),
            AstTypeBase::Int => write!(f, "int"),
            AstTypeBase::Long => write!(f, "long"),
            AstTypeBase::Float => write!(f, "float"),
            AstTypeBase::Double => write!(f, "double"),
            AstTypeBase::Signed => write!(f, "signed"),
            AstTypeBase::Unsigned => write!(f, "unsigned"),
            AstTypeBase::Union(_) | AstTypeBase::Enum => panic!(),
        }
    }
}

//=ast_type_str_build_struct
impl Display for AstStructType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        assert!(self.tag.is_some() || self.members.is_some());
        write!(f, "struct ")?;
        if let Some(tag) = &self.tag {
            write!(f, "{tag}")?;
        }
        let Some(members) = self.members.as_ref() else {
            return Ok(());
        };
        write!(f, " {{ ")?;
        for field in members.iter() {
            write!(f, "{field}; ")?;
        }
        write!(f, "}}")
    }
}

fn mod_str(modifiers: AstTypeModifiers) -> String {
    let modstr: [&'static str; 7] = [
        "typedef", "extern", "static", "auto", "register", "const", "volatile",
    ];
    let mut b = String::new();
    for (i, name) in modstr.iter().enumerate() {
        if 1 << i & modifiers != 0 {
            // Note: The original does some unnecessary work to decide whether to add a space, but
            // the answer is always yes, add the space. This is on purpose because of how this
            // function is used.
            str_write!(b, "{name} ");
        }
    }
    b
}

pub fn ast_type_create(base: AstTypeBase, modifiers: AstTypeModifiers) -> Box<AstType> {
    Box::new(AstType { base, modifiers })
}

pub fn ast_type_create_ptr(referent: Box<AstType>) -> Box<AstType> {
    ast_type_create(AstTypeBase::Pointer(referent), 0)
}

pub fn ast_type_create_voidptr() -> Box<AstType> {
    ast_type_create(
        AstTypeBase::Pointer(ast_type_create(AstTypeBase::Void, 0)),
        0,
    )
}

#[allow(dead_code)]
pub fn ast_type_create_arr(base: Box<AstType>, length: c_int) -> Box<AstType> {
    ast_type_create(
        AstTypeBase::Array(AstArrayType {
            type_: base,
            length,
        }),
        0,
    )
}

pub fn ast_type_create_struct(
    tag: Option<String>,
    members: Option<Box<Vec<Box<AstVariable>>>>,
) -> Box<AstType> {
    ast_type_create(AstTypeBase::Struct(AstStructType { tag, members }), 0)
}

pub fn ast_type_create_userdef(name: String) -> Box<AstType> {
    ast_type_create(AstTypeBase::UserDefined(name), 0)
}

pub fn ast_type_vconst(t: &AstType, s: &mut State, comment: &str, persist: bool) -> Box<Value> {
    match &t.base {
        AstTypeBase::Int => Value::new_int_indefinite(),
        AstTypeBase::Pointer(_) => Value::new_ptr_indefinite(),
        AstTypeBase::UserDefined(name) => {
            let ext = s.ext();
            let type_ = ext.get_typedef(name).unwrap();
            ast_type_vconst(
                // Note: Original does not null-check here.
                type_, s, comment, persist,
            )
        }
        AstTypeBase::Struct(_) => Value::new_struct_indefinite(t, s, comment, persist),
        _ => panic!(),
    }
}

pub fn ast_type_isstruct(t: &AstType) -> bool {
    matches!(t.base, AstTypeBase::Struct(_))
}

pub fn ast_type_struct_complete<'a>(t: &'a AstType, ext: &'a Externals) -> Option<&'a AstType> {
    if ast_type_struct_members(t).is_some() {
        return Some(t);
    }
    let Some(tag) = ast_type_struct_tag(t) else {
        panic!();
    };
    ext.get_struct(tag)
}

pub fn ast_type_struct_members(t: &AstType) -> Option<&[Box<AstVariable>]> {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!();
    };
    s.members.as_ref().map(|v| v.as_slice())
}

pub fn ast_type_struct_tag(t: &AstType) -> Option<&str> {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!();
    };
    s.tag.as_deref()
}

pub fn ast_type_create_struct_anonym(members: Vec<Box<AstVariable>>) -> Box<AstType> {
    ast_type_create_struct(None, Some(Box::new(members)))
}

pub fn ast_type_create_struct_partial(tag: String) -> Box<AstType> {
    ast_type_create_struct(Some(tag), None)
}

pub fn ast_type_mod_or(t: &mut AstType, m: AstTypeModifiers) {
    t.modifiers |= m;
}

pub fn ast_type_istypedef(t: &AstType) -> bool {
    t.modifiers & MOD_TYPEDEF != 0
}

pub fn ast_type_copy(t: &AstType) -> Box<AstType> {
    Box::new(t.clone())
}

pub fn ast_type_ptr_type(t: &AstType) -> &AstType {
    let AstTypeBase::Pointer(ptr_type) = &t.base else {
        panic!();
    };
    ptr_type
}
