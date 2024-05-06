use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use crate::ast::{ast_expr_copy, ast_expr_equal};
use crate::state::location::{location_references, location_referencesheap};
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
            let name = &var.name;
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

impl Display for NumberRange {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_single() {
            write!(f, "{}", self.lower)
        } else {
            write!(f, "{}:{}", self.lower, self.upper)
        }
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

impl Value {
    fn new(kind: ValueKind) -> Box<Value> {
        Box::new(Value { kind })
    }

    //=value_ptr_create
    pub fn new_ptr(loc: Box<Location>) -> Box<Value> {
        Value::new(ValueKind::DefinitePtr(loc))
    }

    //=value_ptr_indefinite_create
    pub fn new_ptr_indefinite() -> Box<Value> {
        Value::new(ValueKind::IndefinitePtr(Number::new_indefinite()))
    }

    //=value_int_create
    pub fn new_int(val: i32) -> Box<Value> {
        Value::new(ValueKind::Int(Number::new_single(val)))
    }

    //=value_literal_create
    pub fn new_literal(lit: &str) -> Box<Value> {
        Value::new(ValueKind::Literal(lit.to_string()))
    }

    //=value_int_ne_create
    #[allow(dead_code)]
    pub fn new_int_ne(not_val: i32) -> Box<Value> {
        Value::new(ValueKind::Int(Number::new_ne(not_val)))
    }

    //=value_int_range_create
    #[allow(dead_code)]
    pub fn new_int_range(lw: i32, excl_up: i32) -> Box<Value> {
        Value::new(ValueKind::Int(Number::new_range(lw, excl_up)))
    }

    //=value_int_indefinite_create
    pub fn new_int_indefinite() -> Box<Value> {
        Value::new(ValueKind::Int(Number::new_indefinite()))
    }

    //=value_sync_create
    pub fn new_sync(e: Box<AstExpr>) -> Box<Value> {
        Value::new(ValueKind::Sync(Number::new_computed(e)))
    }

    //=value_struct_create
    pub fn new_struct(t: &AstType) -> Box<Value> {
        let members = t
            .struct_members()
            .expect("can't create value of incomplete type");
        Value::new(ValueKind::Struct(Box::new(StructValue {
            members: members.to_vec(),
            m: from_members(members),
        })))
    }

    //=value_struct_indefinite_create
    pub fn new_struct_indefinite(
        t: &AstType,
        s: &mut State,
        comment: &str,
        persist: bool,
    ) -> Box<Value> {
        // Note: The original doesn't null-check here. I wonder how it would handle `typedef struct foo
        // foo;`.
        let t = t.struct_complete(s.ext()).unwrap();
        assert!(t.struct_members().is_some());
        let mut v = Value::new_struct(t);
        let ValueKind::Struct(sv) = &mut v.kind else {
            panic!();
        };
        for var in &sv.members {
            let field = &var.name;

            let obj = sv.m.get_mut(field).unwrap();
            let comment = format!("{comment}.{field}");
            obj.assign(Some(state_vconst(s, &var.type_, Some(&comment), persist)));
        }
        v
    }

    //=value_transfigure
    #[allow(dead_code)]
    pub fn transfigure<'v>(
        &'v self,
        compare: &mut State,
        islval: bool,
    ) -> Option<SemiBox<'v, Value>> {
        match &self.kind {
            ValueKind::Sync(_) | ValueKind::Literal(_) => {
                if islval {
                    None
                } else {
                    Some(SemiBox::Borrowed(self))
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
                        &AstType::new_voidptr(),
                        None,
                        false,
                    )))
                }
            }
            ValueKind::DefinitePtr(loc) => Some(SemiBox::Owned(loc.transfigure(compare))),
            ValueKind::IndefinitePtr(_) => panic!(),
        }
    }

    //=value_pf_augment
    pub fn pf_augment(&self, root: &AstExpr) -> Box<Value> {
        let mut v = Value::copy(self);
        let ValueKind::Struct(sv) = &mut v.kind else {
            panic!();
        };

        for var in &sv.members {
            let field = &var.name;
            let obj = sv.m.get_mut(field).unwrap();
            if let Some(obj_value) = obj.as_value() {
                if obj_value.is_sync() {
                    obj.assign(Some(Value::new_sync(AstExpr::new_member(
                        ast_expr_copy(root),
                        field.to_string(),
                    ))));
                }
            }
        }
        v
    }

    //=value_is_struct
    pub fn is_struct(&self) -> bool {
        matches!(self.kind, ValueKind::Struct(_))
    }

    //=value_struct_member
    pub fn struct_member<'v>(&'v self, member: &str) -> Option<&'v Object> {
        let ValueKind::Struct(sv) = &self.kind else {
            panic!();
        };
        sv.m.get(member).map(|boxed| &**boxed)
    }

    //=value_copy
    pub fn copy(v: &Value) -> Box<Value> {
        Box::new(v.clone())
    }

    //=value_abstractcopy
    pub fn abstract_copy(&self, s: &State) -> Option<Box<Value>> {
        if !self.references_heap(s) {
            return None;
        }
        Some(match &self.kind {
            ValueKind::IndefinitePtr(_) | ValueKind::DefinitePtr(_) => Value::copy(self),
            ValueKind::Struct(sv) => sv.abstract_copy(s),
            _ => panic!(),
        })
    }
}

fn from_members(members: &[Box<AstVariable>]) -> HashMap<String, Box<Object>> {
    let mut m = HashMap::new();
    for var in members {
        let id = var.name.clone();
        m.insert(id, Object::with_value(AstExpr::new_constant(0), None));
    }
    m
}

impl Value {
    //=value_islocation
    pub fn is_location(&self) -> bool {
        matches!(self.kind, ValueKind::DefinitePtr(_))
    }

    //=value_as_location
    pub fn as_location(&self) -> &Location {
        let ValueKind::DefinitePtr(loc) = &self.kind else {
            panic!();
        };
        loc
    }

    pub fn into_location(self) -> Box<Location> {
        let ValueKind::DefinitePtr(loc) = self.kind else {
            panic!();
        };
        loc
    }

    //=value_referencesheap
    pub fn references_heap(&self, s: &State) -> bool {
        match &self.kind {
            ValueKind::DefinitePtr(loc) => location_referencesheap(loc, s),
            ValueKind::IndefinitePtr(_) => false,
            ValueKind::Struct(sv) => sv.references_heap(s),
            _ => false,
        }
    }

    //=value_isconstant
    //=value_as_constant
    pub fn as_constant(&self) -> Option<i32> {
        let ValueKind::Int(n) = &self.kind else {
            return None;
        };
        n.as_constant()
    }

    //=value_issync
    pub fn is_sync(&self) -> bool {
        match &self.kind {
            ValueKind::Sync(n) => n.is_sync(),
            _ => false,
        }
    }

    //=value_as_sync
    pub fn as_sync(&self) -> &AstExpr {
        let ValueKind::Sync(n) = &self.kind else {
            panic!();
        };
        n.as_sync()
    }

    pub fn into_sync(self) -> Box<AstExpr> {
        let ValueKind::Sync(n) = self.kind else {
            panic!();
        };
        n.into_sync()
    }

    //=value_isint
    pub fn is_int(&self) -> bool {
        matches!(self.kind, ValueKind::Int(_))
    }

    //=value_to_expr
    pub fn to_expr(&self) -> Box<AstExpr> {
        // Note: In the original, the return value from `value_as_literal` was redundantly copied and
        // then leaked.
        match &self.kind {
            ValueKind::DefinitePtr(_) => AstExpr::new_identifier(self.to_string()),
            ValueKind::IndefinitePtr(_) => AstExpr::new_identifier(self.to_string()),
            ValueKind::Literal(s) => AstExpr::new_literal(s.clone()),
            ValueKind::Sync(n) => ast_expr_copy(n.as_sync()),
            ValueKind::Int(n) => n.to_expr(),
            _ => panic!(),
        }
    }

    //=value_references
    pub fn references(&self, loc: &Location, s: &State) -> bool {
        match &self.kind {
            ValueKind::DefinitePtr(vloc) => location_references(vloc, loc, s),
            ValueKind::Struct(sv) => sv.references(loc, s),
            _ => false,
        }
    }

    //=value_equal
    pub fn equal(v1: &Value, v2: &Value) -> bool {
        match (&v1.kind, &v2.kind) {
            (ValueKind::Literal(s1), ValueKind::Literal(s2)) => s1 == s2,
            (ValueKind::Sync(n1), ValueKind::Sync(n2))
            | (ValueKind::Int(n1), ValueKind::Int(n2)) => Number::equal(n1, n2),
            _ => panic!(),
        }
    }

    //=value_assume
    #[allow(dead_code)]
    pub fn value_assume(&mut self, value: bool) -> bool {
        match &mut self.kind {
            ValueKind::Int(n) => n.assume(value),
            ValueKind::IndefinitePtr(n) => n.assume(value),
            _ => panic!(),
        }
    }
}

impl StructValue {
    //=value_struct_abstractcopy
    fn abstract_copy(&self, s: &State) -> Box<Value> {
        Value::new(ValueKind::Struct(Box::new(StructValue {
            members: self.members.clone(),
            //=abstract_copy_members
            m: self
                .m
                .iter()
                .map(|(k, obj)| (k.clone(), obj.abstract_copy(s)))
                .collect(),
        })))
    }

    //=struct_referencesheap
    fn references_heap(&self, s: &State) -> bool {
        self.m.values().any(|obj| {
            let Some(val) = obj.as_value() else {
                return false;
            };
            val.references_heap(s)
        })
    }

    //=struct_references
    fn references(&self, loc: &Location, s: &State) -> bool {
        self.m.values().any(|obj| {
            let Some(val) = obj.as_value() else {
                return false;
            };
            val.references(loc, s)
        })
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
        Number::from_ranges(vec![NumberRange::new(
            NumberValue::Constant(lw),
            NumberValue::Constant(excl_up),
        )])
    }

    fn new_indefinite() -> Box<Number> {
        Number::from_ranges(vec![NumberRange::new(NumberValue::MIN, NumberValue::MAX)])
    }

    fn new_computed(e: Box<AstExpr>) -> Box<Self> {
        Number::new(NumberKind::Computed(e))
    }

    //=number_equal
    fn equal(n1: &Number, n2: &Number) -> bool {
        match (&n1.kind, &n2.kind) {
            (NumberKind::Ranges(ranges1), NumberKind::Ranges(ranges2)) => ranges1 == ranges2,
            (NumberKind::Computed(c1), NumberKind::Computed(c2)) => ast_expr_equal(c1, c2),
            _ => panic!(),
        }
    }

    //=number_assume
    #[allow(dead_code)]
    fn assume(&mut self, value: bool) -> bool {
        let NumberKind::Ranges(ranges) = &mut self.kind else {
            panic!();
        };

        if !ranges.iter().any(|range| range.can_be(value)) {
            return false;
        }
        *ranges = NumberRange::assumed_value(value);
        true
    }

    //=number_isconstant
    //=number_as_constant
    fn as_constant(&self) -> Option<i32> {
        let NumberKind::Ranges(ranges) = &self.kind else {
            return None;
        };
        if ranges.len() == 1 && ranges[0].is_single() {
            Some(ranges[0].as_constant())
        } else {
            None
        }
    }

    //=number_issync
    fn is_sync(&self) -> bool {
        matches!(self.kind, NumberKind::Computed(_))
    }

    //=number_as_sync
    fn as_sync(&self) -> &AstExpr {
        let NumberKind::Computed(computation) = &self.kind else {
            panic!();
        };
        computation
    }

    fn into_sync(self) -> Box<AstExpr> {
        let NumberKind::Computed(computation) = self.kind else {
            panic!();
        };
        computation
    }

    //=number_to_expr
    fn to_expr(&self) -> Box<AstExpr> {
        match &self.kind {
            NumberKind::Ranges(ranges) => number_ranges_to_expr(ranges),
            NumberKind::Computed(computation) => ast_expr_copy(computation),
        }
    }
}

impl NumberRange {
    //=number_range_assumed_value
    fn assumed_value(value: bool) -> Vec<NumberRange> {
        if value {
            NumberRange::arr_ne_create(0)
        } else {
            NumberRange::arr_single_create(0)
        }
    }

    //=number_range_create
    fn new(lw: NumberValue, up: NumberValue) -> NumberRange {
        NumberRange {
            lower: lw,
            upper: up,
        }
    }
}

fn number_ranges_to_expr(arr: &[NumberRange]) -> Box<AstExpr> {
    assert_eq!(arr.len(), 1);
    AstExpr::new_constant(arr[0].as_constant())
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
        // Note: Possibility of overflow ignored in original.
        vec![NumberRange::new(
            NumberValue::Constant(val),
            NumberValue::Constant(val + 1),
        )]
    }

    fn arr_ne_create(val: i32) -> Vec<NumberRange> {
        vec![
            NumberRange::new(NumberValue::MIN, NumberValue::Constant(val)),
            NumberRange::new(NumberValue::Constant(val + 1), NumberValue::MAX),
        ]
    }

    //=number_range_as_constant
    fn as_constant(&self) -> i32 {
        if !self.is_single() {
            panic!();
        }
        match self.lower {
            NumberValue::Constant(k) => k,
            _ => panic!(),
        }
    }
}

impl NumberValue {
    const MIN: Self = NumberValue::Limit(false);
    const MAX: Self = NumberValue::Limit(true);

    #[allow(dead_code)]
    fn difference(v1: &NumberValue, v2: &NumberValue) -> i32 {
        match (*v1, *v2) {
            (NumberValue::Constant(v1), NumberValue::Constant(v2)) => v1 - v2,
            _ => panic!(),
        }
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

fn number_values_aresingle(v1: &NumberValue, v2: &NumberValue) -> bool {
    match (*v1, *v2) {
        (NumberValue::Constant(k1), NumberValue::Constant(k2)) => k1 == k2 - 1,
        (NumberValue::Limit(max1), NumberValue::Limit(max2)) => max1 == max2,
        _ => panic!(),
    }
}
