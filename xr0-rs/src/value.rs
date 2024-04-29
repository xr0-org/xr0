use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use crate::ast::{
    ast_expr_copy, ast_expr_equal, ast_type_create_voidptr, ast_type_struct_complete,
    ast_type_struct_members, ast_variable_arr_copy, ast_variable_name, ast_variable_type,
};
use crate::state::location::{location_references, location_referencesheap, location_transfigure};
use crate::state::state::state_vconst;
use crate::state::State;
use crate::util::SemiBox;
use crate::{AstExpr, AstType, AstVariable, Location, Object};

#[derive(Clone)]
pub struct Value {
    pub kind: ValueKind,
}

#[derive(Clone)]
pub enum ValueKind {
    Sync(Box<Number>),
    IndefinitePtr(Box<Number>),
    DefinitePtr(Box<Location>),
    Int(Box<Number>),
    Literal(String),
    Struct(Box<StructValue>),
}

#[derive(Clone)]
pub struct StructValue {
    pub members: Vec<Box<AstVariable>>,
    pub m: HashMap<String, Box<Object>>,
}

#[derive(Clone)]
pub struct Number {
    pub kind: NumberKind,
}

#[derive(Clone)]
pub enum NumberKind {
    Ranges(Vec<NumberRange>),
    Computed(Box<AstExpr>),
}

#[derive(Copy, Clone, PartialEq)]
pub struct NumberRange {
    pub lower: NumberValue,
    pub upper: NumberValue,
}

#[derive(Copy, Clone, PartialEq)]
pub enum NumberValue {
    Constant(i32),
    Limit(bool),
}

impl Value {
    fn new(kind: ValueKind) -> Box<Value> {
        Box::new(Value { kind })
    }
}

pub fn value_ptr_create(loc: Box<Location>) -> Box<Value> {
    Value::new(ValueKind::DefinitePtr(loc))
}

pub fn value_ptr_indefinite_create() -> Box<Value> {
    Value::new(ValueKind::IndefinitePtr(Number::new_indefinite()))
}

pub fn value_int_create(val: i32) -> Box<Value> {
    Value::new(ValueKind::Int(Number::new_single(val)))
}

pub fn value_literal_create(lit: &str) -> Box<Value> {
    Value::new(ValueKind::Literal(lit.to_string()))
}

#[allow(dead_code)]
pub fn value_int_ne_create(not_val: i32) -> Box<Value> {
    Value::new(ValueKind::Int(Number::new_ne(not_val)))
}

#[allow(dead_code)]
pub fn value_int_range_create(lw: i32, excl_up: i32) -> Box<Value> {
    Value::new(ValueKind::Int(Number::new_range(lw, excl_up)))
}

pub fn value_int_indefinite_create() -> Box<Value> {
    Value::new(ValueKind::Int(Number::new_indefinite()))
}

pub fn value_sync_create(e: Box<AstExpr>) -> Box<Value> {
    Value::new(ValueKind::Sync(Number::new_computed(e)))
}

pub fn value_struct_create(t: &AstType) -> Box<Value> {
    let members = ast_type_struct_members(t).expect("can't create value of incomplete type");
    Value::new(ValueKind::Struct(Box::new(StructValue {
        members: ast_variable_arr_copy(members),
        m: from_members(members),
    })))
}

pub fn value_struct_indefinite_create(
    t: &AstType,
    s: &mut State,
    comment: &str,
    persist: bool,
) -> Box<Value> {
    // Note: The original doesn't null-check here. I wonder how it would handle `typedef struct foo
    // foo;`.
    let t = ast_type_struct_complete(t, s.ext()).unwrap();
    assert!(ast_type_struct_members(t).is_some());
    let mut v = value_struct_create(t);
    let ValueKind::Struct(sv) = &mut v.kind else {
        panic!();
    };
    for var in &sv.members {
        let field = ast_variable_name(var);

        let obj = sv.m.get_mut(field).unwrap();
        let comment = format!("{comment}.{field}");
        obj.assign(Some(state_vconst(
            s,
            ast_variable_type(var),
            Some(&comment),
            persist,
        )));
    }
    v
}

#[allow(dead_code)]
pub fn value_transfigure<'v>(
    v: &'v Value,
    compare: &mut State,
    islval: bool,
) -> Option<SemiBox<'v, Value>> {
    match &v.kind {
        ValueKind::Sync(_) | ValueKind::Literal(_) => {
            if islval {
                None
            } else {
                Some(SemiBox::Borrowed(v))
            }
        }
        ValueKind::Struct(_) => {
            panic!();
        }
        ValueKind::Int(_) => {
            if islval {
                None
            } else {
                // Note: Original leaked this type.
                Some(SemiBox::Owned(state_vconst(
                    &mut *compare,
                    &ast_type_create_voidptr(),
                    None,
                    false,
                )))
            }
        }
        ValueKind::DefinitePtr(loc) => {
            Some(SemiBox::Owned(location_transfigure(loc, &mut *compare)))
        }
        ValueKind::IndefinitePtr(_) => panic!(),
    }
}

pub fn value_pf_augment(old: &Value, root: &AstExpr) -> Box<Value> {
    let mut v = value_copy(old);
    let ValueKind::Struct(sv) = &mut v.kind else {
        panic!();
    };

    for var in &sv.members {
        let field = ast_variable_name(var);
        let obj = sv.m.get_mut(field).unwrap();
        if let Some(obj_value) = obj.as_value() {
            if value_issync(obj_value) {
                obj.assign(Some(value_sync_create(AstExpr::new_member(
                    ast_expr_copy(root),
                    field.to_string(),
                ))));
            }
        }
    }
    v
}

pub fn value_isstruct(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Struct(_))
}

fn from_members(members: &[Box<AstVariable>]) -> HashMap<String, Box<Object>> {
    let mut m = HashMap::new();
    for var in members {
        let id = ast_variable_name(var).to_string();
        m.insert(id, Object::with_value(AstExpr::new_constant(0), None));
    }
    m
}

fn value_struct_abstractcopy(old: &StructValue, s: &mut State) -> Box<Value> {
    Value::new(ValueKind::Struct(Box::new(StructValue {
        members: ast_variable_arr_copy(&old.members),
        m: abstract_copy_members(&old.m, s),
    })))
}

fn abstract_copy_members(
    old: &HashMap<String, Box<Object>>,
    s: &mut State,
) -> HashMap<String, Box<Object>> {
    old.iter()
        .map(|(k, obj)| (k.clone(), obj.abstract_copy(s)))
        .collect()
}

pub fn value_struct_member<'v>(v: &'v Value, member: &str) -> Option<&'v Object> {
    let ValueKind::Struct(sv) = &v.kind else {
        panic!();
    };
    sv.m.get(member).map(|boxed| &**boxed)
}

pub fn value_copy(v: &Value) -> Box<Value> {
    Box::new(v.clone())
}

pub fn value_abstractcopy(v: &Value, s: &mut State) -> Option<Box<Value>> {
    if !value_referencesheap(v, s) {
        return None;
    }
    Some(match &v.kind {
        ValueKind::IndefinitePtr(_) | ValueKind::DefinitePtr(_) => value_copy(v),
        ValueKind::Struct(sv) => value_struct_abstractcopy(sv, s),
        _ => panic!(),
    })
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            ValueKind::Sync(n) => write!(f, "comp:{n}"),
            ValueKind::DefinitePtr(loc) => write!(f, "ptr:{loc}"),
            ValueKind::IndefinitePtr(n) => write!(f, "ptr:{n}"),
            ValueKind::Int(n) => write!(f, "int:{n}"),
            ValueKind::Literal(s) => write!(f, "\"{s}\""),
            ValueKind::Struct(sv) => write!(f, "{sv}"),
        }
    }
}

impl Display for StructValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "struct:{{")?;
        let n = self.members.len();
        for (i, var) in self.members.iter().enumerate() {
            let name = ast_variable_name(var);
            let val = self.m.get(name).unwrap().as_value();
            let val_str = if let Some(val) = val {
                format!("{val}")
            } else {
                "".to_string()
            };
            let delim = if i + 1 < n { ", " } else { "" };
            write!(f, ".{name} = <{val_str}>{delim}")?;
        }
        write!(f, "}}")
    }
}

pub fn value_islocation(v: &Value) -> bool {
    matches!(v.kind, ValueKind::DefinitePtr(_))
}

pub fn value_as_location(v: &Value) -> &Location {
    let ValueKind::DefinitePtr(loc) = &v.kind else {
        panic!();
    };
    loc
}

impl Value {
    pub fn into_location(self) -> Box<Location> {
        let ValueKind::DefinitePtr(loc) = self.kind else {
            panic!();
        };
        loc
    }
}

pub fn value_referencesheap(v: &Value, s: &mut State) -> bool {
    match &v.kind {
        ValueKind::DefinitePtr(loc) => location_referencesheap(loc, s),
        ValueKind::IndefinitePtr(_) => false,
        ValueKind::Struct(sv) => struct_referencesheap(sv, s),
        _ => false,
    }
}

fn struct_referencesheap(sv: &StructValue, s: &mut State) -> bool {
    sv.m.values().any(|obj| {
        let Some(val) = obj.as_value() else {
            return false;
        };
        value_referencesheap(val, s)
    })
}

pub fn value_as_constant(v: &Value) -> i32 {
    let ValueKind::Int(n) = &v.kind else {
        panic!();
    };
    number_as_constant(n)
}

pub fn value_isconstant(v: &Value) -> bool {
    match &v.kind {
        ValueKind::Int(n) => number_isconstant(n),
        _ => false,
    }
}

pub fn value_issync(v: &Value) -> bool {
    match &v.kind {
        ValueKind::Sync(n) => number_issync(n),
        _ => false,
    }
}

pub fn value_as_sync(v: &Value) -> &AstExpr {
    let ValueKind::Sync(n) = &v.kind else {
        panic!();
    };
    number_as_sync(n)
}

impl Value {
    pub fn into_sync(self) -> Box<AstExpr> {
        let ValueKind::Sync(n) = self.kind else {
            panic!();
        };
        number_into_sync(*n)
    }
}

pub fn value_isint(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Int(_))
}

pub fn value_to_expr(v: &Value) -> Box<AstExpr> {
    // Note: In the original, the return value from `value_as_literal` was redundantly copied and
    // then leaked.
    match &v.kind {
        ValueKind::DefinitePtr(_) => AstExpr::new_identifier(v.to_string()),
        ValueKind::IndefinitePtr(_) => AstExpr::new_identifier(v.to_string()),
        ValueKind::Literal(_) => value_as_literal(v),
        ValueKind::Sync(n) => ast_expr_copy(number_as_sync(n)),
        ValueKind::Int(n) => number_to_expr(n),
        _ => panic!(),
    }
}

#[allow(dead_code)]
pub fn value_isliteral(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Literal(_))
}

pub fn value_as_literal(v: &Value) -> Box<AstExpr> {
    let ValueKind::Literal(s) = &v.kind else {
        panic!();
    };
    AstExpr::new_literal(s.clone())
}

pub fn value_references(v: &Value, loc: &Location, s: &mut State) -> bool {
    match &v.kind {
        ValueKind::DefinitePtr(vloc) => location_references(vloc, loc, s),
        ValueKind::Struct(sv) => struct_references(sv, loc, s),
        _ => false,
    }
}

fn struct_references(sv: &StructValue, loc: &Location, s: &mut State) -> bool {
    sv.m.values().any(|obj| {
        let Some(val) = obj.as_value() else {
            return false;
        };
        value_references(val, loc, s)
    })
}

pub fn value_equal(v1: &Value, v2: &Value) -> bool {
    match (&v1.kind, &v2.kind) {
        (ValueKind::Literal(s1), ValueKind::Literal(s2)) => s1 == s2,
        (ValueKind::Sync(n1), ValueKind::Sync(n2)) | (ValueKind::Int(n1), ValueKind::Int(n2)) => {
            number_equal(n1, n2)
        }
        _ => panic!(),
    }
}

#[allow(dead_code)]
pub fn value_assume(v: &mut Value, value: bool) -> bool {
    match &mut v.kind {
        ValueKind::Int(n) => number_assume(n, value),
        ValueKind::IndefinitePtr(n) => number_assume(n, value),
        _ => panic!(),
    }
}

impl Number {
    fn new(kind: NumberKind) -> Box<Number> {
        Box::new(Number { kind })
    }

    fn from_ranges(ranges: Vec<NumberRange>) -> Box<Number> {
        Number::new(NumberKind::Ranges(ranges))
    }

    fn new_single(val: i32) -> Box<Number> {
        Number::from_ranges(NumberRange::arr_single_create(val))
    }

    fn new_ne(val: i32) -> Box<Number> {
        Number::from_ranges(NumberRange::arr_ne_create(val))
    }

    fn new_range(lw: i32, excl_up: i32) -> Box<Number> {
        Number::from_ranges(vec![number_range_create(
            NumberValue::Constant(lw),
            NumberValue::Constant(excl_up),
        )])
    }

    fn new_indefinite() -> Box<Number> {
        Number::from_ranges(vec![number_range_create(
            NumberValue::MIN,
            NumberValue::MAX,
        )])
    }

    fn new_computed(e: Box<AstExpr>) -> Box<Self> {
        Number::new(NumberKind::Computed(e))
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            NumberKind::Ranges(ranges) => {
                write!(f, "{{")?;
                let n = ranges.len();
                for (i, range) in ranges.iter().enumerate() {
                    write!(f, "{range}")?;
                    if i + 1 < n {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            NumberKind::Computed(computation) => write!(f, "{computation}"),
        }
    }
}

fn number_equal(n1: &Number, n2: &Number) -> bool {
    match (&n1.kind, &n2.kind) {
        (NumberKind::Ranges(ranges1), NumberKind::Ranges(ranges2)) => ranges1 == ranges2,
        (NumberKind::Computed(c1), NumberKind::Computed(c2)) => ast_expr_equal(c1, c2),
        _ => panic!(),
    }
}

fn number_assume(n: &mut Number, value: bool) -> bool {
    let NumberKind::Ranges(ranges) = &mut n.kind else {
        panic!();
    };

    if !ranges.iter().any(|range| range.can_be(value)) {
        return false;
    }
    *ranges = number_range_assumed_value(value);
    true
}

fn number_range_assumed_value(value: bool) -> Vec<NumberRange> {
    if value {
        NumberRange::arr_ne_create(0)
    } else {
        NumberRange::arr_single_create(0)
    }
}

fn number_isconstant(n: &Number) -> bool {
    let NumberKind::Ranges(ranges) = &n.kind else {
        panic!();
    };
    ranges.len() == 1 && ranges[0].is_single()
}

fn number_as_constant(n: &Number) -> i32 {
    let NumberKind::Ranges(ranges) = &n.kind else {
        panic!();
    };
    assert_eq!(ranges.len(), 1);
    number_range_as_constant(&ranges[0])
}

fn number_issync(n: &Number) -> bool {
    matches!(n.kind, NumberKind::Computed(_))
}

fn number_as_sync(n: &Number) -> &AstExpr {
    let NumberKind::Computed(computation) = &n.kind else {
        panic!();
    };
    computation
}

fn number_into_sync(n: Number) -> Box<AstExpr> {
    let NumberKind::Computed(computation) = n.kind else {
        panic!();
    };
    computation
}

fn number_to_expr(n: &Number) -> Box<AstExpr> {
    match &n.kind {
        NumberKind::Ranges(ranges) => number_ranges_to_expr(ranges),
        NumberKind::Computed(computation) => ast_expr_copy(computation),
    }
}

fn number_ranges_to_expr(arr: &[NumberRange]) -> Box<AstExpr> {
    assert_eq!(arr.len(), 1);
    AstExpr::new_constant(number_range_as_constant(&arr[0]))
}

fn number_range_create(lw: NumberValue, up: NumberValue) -> NumberRange {
    NumberRange {
        lower: lw,
        upper: up,
    }
}

impl Display for NumberRange {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_single() {
            write!(f, "{}", self.lower)
        } else {
            write!(f, "{}:{}", self.lower, self.upper)
        }
    }
}

impl NumberRange {
    fn can_be(&self, value: bool) -> bool {
        if value {
            if self.lower == self.upper {
                return false;
            }
            number_value_le_constant(self.lower, -1) || constant_le_number_value(1, self.lower)
        } else {
            number_value_le_constant(self.lower, 0) && constant_le_number_value(1, self.upper)
        }
    }

    fn is_single(&self) -> bool {
        number_values_aresingle(&self.lower, &self.upper)
    }

    fn arr_single_create(val: i32) -> Vec<NumberRange> {
        vec![number_range_create(
            NumberValue::Constant(val),
            NumberValue::Constant(val + 1),
        )]
    }

    fn arr_ne_create(val: i32) -> Vec<NumberRange> {
        vec![
            number_range_create(NumberValue::MIN, NumberValue::Constant(val)),
            number_range_create(NumberValue::Constant(val + 1), NumberValue::MAX),
        ]
    }
}

fn number_range_as_constant(r: &NumberRange) -> i32 {
    if !r.is_single() {
        panic!();
    }
    match r.lower {
        NumberValue::Constant(k) => k,
        _ => panic!(),
    }
}

impl Display for NumberValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            NumberValue::Constant(k) => write!(f, "{k}"),
            NumberValue::Limit(true) => write!(f, "MAX"),
            NumberValue::Limit(false) => write!(f, "MIN"),
        }
    }
}

#[allow(dead_code)]
fn number_value_difference(v1: &NumberValue, v2: &NumberValue) -> i32 {
    match (*v1, *v2) {
        (NumberValue::Constant(v1), NumberValue::Constant(v2)) => v1 - v2,
        _ => panic!(),
    }
}

fn number_value_le_constant(v: NumberValue, constant: i32) -> bool {
    match v {
        NumberValue::Constant(k) => k <= constant,
        NumberValue::Limit(max) => !max,
    }
}

fn constant_le_number_value(constant: i32, v: NumberValue) -> bool {
    match v {
        NumberValue::Constant(k) => constant <= k,
        NumberValue::Limit(max) => max,
    }
}

impl NumberValue {
    const MIN: Self = NumberValue::Limit(false);
    const MAX: Self = NumberValue::Limit(true);
}

fn number_values_aresingle(v1: &NumberValue, v2: &NumberValue) -> bool {
    match (*v1, *v2) {
        (NumberValue::Constant(k1), NumberValue::Constant(k2)) => k1 == k2 - 1,
        (NumberValue::Limit(max1), NumberValue::Limit(max2)) => max1 == max2,
        _ => panic!(),
    }
}
