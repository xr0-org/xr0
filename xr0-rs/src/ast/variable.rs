use std::fmt::{self, Display, Formatter};

use super::AstType;

#[derive(Clone)]
pub struct AstVariable {
    // Note: In the original, `name` could be null. However in most situations (almost all; an
    // exception is the parameter in `fclose(FILE *);` which has no name) this would be invalid, so
    // we end up banning it.
    pub name: String,
    pub type_: Box<AstType>,
}

//=ast_variable_str
impl Display for AstVariable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let AstVariable { type_, name } = self;
        write!(f, "{type_} {name}")
    }
}

impl AstVariable {
    pub fn new(name: String, type_: Box<AstType>) -> Box<AstVariable> {
        Box::new(AstVariable { name, type_ })
    }
}
