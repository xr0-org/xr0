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

impl AstType {
    //=ast_type_create
    pub fn new(base: AstTypeBase, modifiers: AstTypeModifiers) -> Box<AstType> {
        Box::new(AstType { base, modifiers })
    }

    //=ast_type_create_ptr
    pub fn new_ptr(referent: Box<AstType>) -> Box<AstType> {
        AstType::new(AstTypeBase::Pointer(referent), 0)
    }

    //=ast_type_create_voidptr
    pub fn new_voidptr() -> Box<AstType> {
        AstType::new(AstTypeBase::Pointer(AstType::new(AstTypeBase::Void, 0)), 0)
    }

    //=ast_type_create_arr
    #[allow(dead_code)]
    pub fn new_arr(base: Box<AstType>, length: c_int) -> Box<AstType> {
        AstType::new(
            AstTypeBase::Array(AstArrayType {
                type_: base,
                length,
            }),
            0,
        )
    }

    //=ast_type_create_struct
    pub fn new_struct(
        tag: Option<String>,
        members: Option<Box<Vec<Box<AstVariable>>>>,
    ) -> Box<AstType> {
        AstType::new(AstTypeBase::Struct(AstStructType { tag, members }), 0)
    }

    //=ast_type_create_userdef
    pub fn new_userdef(name: String) -> Box<AstType> {
        AstType::new(AstTypeBase::UserDefined(name), 0)
    }

    //=ast_type_create_struct_anonym
    pub fn new_struct_anonym(members: Vec<Box<AstVariable>>) -> Box<AstType> {
        AstType::new_struct(None, Some(Box::new(members)))
    }

    //=ast_type_create_struct_partial
    pub fn new_struct_partial(tag: String) -> Box<AstType> {
        AstType::new_struct(Some(tag), None)
    }

    //=ast_type_vconst
    pub fn vconst(&self, s: &mut State, comment: &str, persist: bool) -> Box<Value> {
        match &self.base {
            AstTypeBase::Int => Value::new_int_indefinite(),
            AstTypeBase::Pointer(_) => Value::new_ptr_indefinite(),
            AstTypeBase::UserDefined(name) => {
                let ext = s.ext();
                let type_ = ext.get_typedef(name).unwrap();
                type_.vconst(
                    // Note: Original does not null-check here.
                    s, comment, persist,
                )
            }
            AstTypeBase::Struct(_) => Value::new_struct_indefinite(self, s, comment, persist),
            _ => panic!(),
        }
    }

    //=ast_type_isstruct
    pub fn is_struct(&self) -> bool {
        matches!(self.base, AstTypeBase::Struct(_))
    }

    //=ast_type_struct_complete
    pub fn struct_complete<'a>(&'a self, ext: &'a Externals) -> Option<&'a AstType> {
        if self.struct_members().is_some() {
            return Some(self);
        }
        let tag = self.struct_tag().unwrap();
        ext.get_struct(tag)
    }

    //=ast_type_struct_members
    pub fn struct_members(&self) -> Option<&[Box<AstVariable>]> {
        let AstTypeBase::Struct(s) = &self.base else {
            panic!();
        };
        s.members.as_ref().map(|v| v.as_slice())
    }

    //=ast_type_struct_tag
    pub fn struct_tag(&self) -> Option<&str> {
        let AstTypeBase::Struct(s) = &self.base else {
            panic!();
        };
        s.tag.as_deref()
    }

    //=ast_type_mod_or
    pub fn mod_or(&mut self, m: AstTypeModifiers) {
        self.modifiers |= m;
    }

    //=ast_type_istypedef
    pub fn is_typedef(&self) -> bool {
        self.modifiers & MOD_TYPEDEF != 0
    }

    //=ast_type_ptr_type
    pub fn ptr_type(&self) -> &AstType {
        let AstTypeBase::Pointer(ptr_type) = &self.base else {
            panic!();
        };
        ptr_type
    }
}
