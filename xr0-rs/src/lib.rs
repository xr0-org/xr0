#![deny(warnings)]
#![allow(clippy::box_collection)]
#![allow(clippy::new_without_default)]
#![allow(clippy::vec_box)]

pub mod util;

pub mod ast;
mod ext;
mod math;
mod object;
pub mod parser;
mod path;
mod props;
mod state;
mod value;

pub use ast::{AstExpr, AstFunction, AstType, AstVariable};
pub use ext::Externals;
pub use object::Object;
pub use props::Props;
pub use state::location::Location;
pub use value::Value;
