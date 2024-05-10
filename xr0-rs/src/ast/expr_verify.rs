use super::{
    ast_expr_as_identifier, ast_expr_binary_e1, ast_expr_binary_e2, ast_expr_binary_op,
    ast_expr_copy, ast_expr_equal, ast_expr_incdec_to_assignment, ast_expr_inverted_copy,
    ast_expr_isdeallocand_assertand, ast_expr_unary_op, ast_expr_unary_operand,
    ast_function_absexec, ast_function_initparams, AllocExpr, AssignmentExpr, AstAllocKind,
    AstBinaryOp, AstExpr, AstExprKind, AstFunction, AstType, AstTypeBase, AstUnaryOp, AstVariable,
    BinaryExpr, CallExpr, ConstantExpr, IncDecExpr, StructMemberExpr, UnaryExpr,
};
use crate::state::location::location_copy;
use crate::state::State;
use crate::util::{Error, Result, SemiBox};
use crate::{Object, Value};

pub struct LValue<'ast> {
    pub t: SemiBox<'ast, AstType>,
    pub obj: Option<&'ast mut Object>,
}

#[derive(Clone)]
pub struct Preresult {
    pub is_contradiction: bool,
}

pub fn ast_expr_decide(expr: &AstExpr, state: &mut State) -> bool {
    match &expr.kind {
        AstExprKind::Constant(c) => c.constant != 0,
        AstExprKind::Unary(_) => expr_unary_decide(expr, state),
        AstExprKind::IsDeallocand(_) => expr_isdeallocand_decide(expr, state),
        AstExprKind::Binary(_) => expr_binary_decide(expr, state),
        _ => panic!(),
    }
}

fn expr_unary_decide(expr: &AstExpr, state: &mut State) -> bool {
    let operand = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => !ast_expr_decide(operand, state),
        _ => panic!(),
    }
}

pub fn ast_expr_rangedecide(expr: &AstExpr, lw: &AstExpr, up: &AstExpr, state: &mut State) -> bool {
    match &expr.kind {
        AstExprKind::Unary(_) => unary_rangedecide(expr, lw, up, state),
        AstExprKind::IsDeallocand(_) => expr_isdeallocand_rangedecide(expr, lw, up, state),
        _ => panic!(),
    }
}

fn unary_rangedecide(expr: &AstExpr, lw: &AstExpr, up: &AstExpr, state: &mut State) -> bool {
    let operand = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => !ast_expr_rangedecide(operand, lw, up, state),
        _ => panic!(),
    }
}

fn expr_isdeallocand_rangedecide(
    expr: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> bool {
    let acc = ast_expr_isdeallocand_assertand(expr);
    assert_eq!(ast_expr_unary_op(acc), AstUnaryOp::Dereference);
    let inner = ast_expr_unary_operand(acc);
    let i = AstExpr::new_identifier("i".to_string());
    let j = AstExpr::new_identifier("j".to_string());
    assert!(
        !!(ast_expr_equal(ast_expr_binary_e2(inner), &i)
            || ast_expr_equal(ast_expr_binary_e2(inner), &j))
    );

    // `inner.e1` is a pointer; get its value as a Location.
    //
    // Note: The code looks at `acc.e1` instead of `inner.e1`, a bug in the original. `acc` is
    // treated both as a unary expression and as a binary expression. Therefore this function must
    // currently be dead code.
    let res_lval = ast_expr_lvalue(ast_expr_binary_e1(acc), state).unwrap();
    if ast_expr_equal(lw, up) {
        return true;
    }
    let obj = res_lval.obj.unwrap();
    let Some(arr_val) = obj.as_value() else {
        return false;
    };
    let deref = crate::state::location::location_copy(arr_val.as_location());

    let Some(b) = state.get_block(&deref).unwrap() else {
        return false;
    };
    b.range_aredeallocands(lw, up, state)
}

pub fn ast_expr_alloc_rangeprocess(
    alloc: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> Result<()> {
    // Note: I think both values are leaked in the original.
    let lw_val = ast_expr_eval(lw, state)?;
    let up_val = ast_expr_eval(up, state)?;
    let res_lw = lw_val.to_expr();
    let res_up = up_val.to_expr();
    match &alloc.kind {
        AstExprKind::Assignment(assign) => rangeprocess_alloc(assign, &res_lw, &res_up, state),
        AstExprKind::Allocation(alloc) => rangeprocess_dealloc(alloc, &res_lw, &res_up, state),
        _ => panic!(),
    }
}

fn rangeprocess_alloc(
    assign: &AssignmentExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> Result<()> {
    let AssignmentExpr { lval, rval } = assign;
    let AstExprKind::Allocation(alloc) = &rval.kind else {
        panic!();
    };
    assert_ne!(alloc.kind, AstAllocKind::Dealloc);

    let obj = hack_base_object_from_alloc(lval, state);
    //=state_range_alloc
    let Some(arr_val) = obj.as_value() else {
        return Err(Error::new("no value".to_string()));
    };
    // Rust note: This `location_copy` is for Rust's benefit. Not in the original. Rust doesn't
    // know that `new_block` and `get_block_mut` won't invalidate the location (but they won't).
    let deref = location_copy(arr_val.as_location());

    let new_block = state.heap.new_block();
    let b = state.get_block_mut(&deref).unwrap(); // panic rather than propagate the error - this is in the original
    let Some(b) = b else {
        return Err(Error::new("no block".to_string()));
    };
    assert!(!ast_expr_equal(lw, up));
    b.range_alloc(lw, up, new_block)
}

fn rangeprocess_dealloc(
    dealloc: &AllocExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> Result<()> {
    let obj = hack_base_object_from_alloc(&dealloc.arg, state);

    //=state_range_dealloc
    let Some(arr_val) = obj.as_value() else {
        return Err(Error::new("no value".to_string()));
    };
    // Rust note: This location_copy is not in the original. Needed for Rust safety.
    let deref = location_copy(arr_val.as_location());
    state.dealloc_location_range(&deref, lw, up)
}

/// Given `expr` of the form `*(p+i)`, return the object storing the variable `p`.
fn hack_base_object_from_alloc<'s>(expr: &'s AstExpr, state: &'s mut State) -> &'s Object {
    let inner = ast_expr_unary_operand(expr);
    let i = AstExpr::new_identifier("i".to_string());
    assert!(ast_expr_equal(ast_expr_binary_e2(inner), &i));
    let lval = ast_expr_lvalue(ast_expr_binary_e1(inner), state).unwrap();
    lval.obj.unwrap()
}

pub fn ast_expr_exec(expr: &AstExpr, state: &mut State) -> Result<()> {
    // Note: In the original, the value returned by `ast_expr_eval` is leaked.
    ast_expr_eval(expr, state)?;
    Ok(())
}

pub fn ast_expr_lvalue<'s>(expr: &'s AstExpr, state: &'s mut State) -> Result<LValue<'s>> {
    match &expr.kind {
        AstExprKind::Identifier(id) => expr_identifier_lvalue(id, state),
        AstExprKind::Unary(unary) => expr_unary_lvalue(unary, state),
        AstExprKind::StructMember(sm) => expr_structmember_lvalue(sm, state),
        _ => panic!(),
    }
}

pub fn expr_identifier_lvalue<'s>(id: &str, state: &'s mut State) -> Result<LValue<'s>> {
    state.identifier_lvalue(id)
}

pub fn expr_unary_lvalue<'s>(unary: &'s UnaryExpr, state: &'s mut State) -> Result<LValue<'s>> {
    assert_eq!(unary.op, AstUnaryOp::Dereference);
    let inner = &unary.arg;
    match &inner.kind {
        // XXX: expr for args (scanf()) in function not of form `*(ptr+offset)
        // for some reason
        AstExprKind::Identifier(_) => {
            let root_lval = ast_expr_lvalue(inner, state)?;
            let Some(root_obj) = root_lval.obj else {
                // `root` freed

                // Note: The original does `return (struct lvalue_res) { .lval = NULL, .err = NULL };`
                // here but I believe every single caller dereferences lval without checking it for
                // null, so it will crash.
                panic!();
            };
            // Rust note: Clones in the next two lines are not in the original. They're for Rust's
            // benefit.
            let t = SemiBox::Owned(Box::new(root_lval.t.ptr_type().clone()));
            let root_val = root_obj.as_value().unwrap().clone();

            let obj = state.deref(&root_val, &AstExpr::new_constant(0))?;
            Ok(LValue { t, obj })
        }
        AstExprKind::Binary(BinaryExpr { op: _, e1, e2 }) => {
            let root_lval = ast_expr_lvalue(e1, state)?;
            let Some(root_obj) = root_lval.obj else {
                // `root` freed

                // Note: Original returns null. See note above.
                panic!();
            };
            // Rust note: Clones in the next two lines are not in the original. They're for Rust's
            // benefit.
            let t = SemiBox::Owned(Box::new(root_lval.t.ptr_type().clone()));
            let root_val = root_obj.as_value().unwrap().clone();
            let Ok(res_obj) = state.deref(&root_val, e2) else {
                // Note: Original returns null. See note above.
                panic!();
            };
            Ok(LValue { t, obj: res_obj })
        }
        _ => panic!(),
    }
}

pub fn expr_structmember_lvalue<'s>(
    sm: &'s StructMemberExpr,
    state: &'s mut State,
) -> Result<LValue<'s>> {
    let StructMemberExpr { root, member } = sm;
    let ext = state.ext();

    let root_lval = ast_expr_lvalue(root, state)?;
    let root_obj = root_lval.obj.unwrap();
    let lvalue = root_obj.member_lvalue(&root_lval.t, member, ext);
    if lvalue.obj.is_none() {
        return Err(Error::new(format!("`{root}' has no field `{member}'")));
    }
    Ok(lvalue)
}

fn hack_object_from_assertion<'s>(expr: &'s AstExpr, state: &'s mut State) -> &'s Object {
    let assertand = ast_expr_isdeallocand_assertand(expr);
    ast_expr_lvalue(assertand, state).unwrap().obj.unwrap()
}

fn expr_isdeallocand_decide(expr: &AstExpr, state: &mut State) -> bool {
    let obj = hack_object_from_assertion(expr, state);

    //=state_addresses_deallocand
    // Note: Original doesn't null-check.
    let val = obj.as_value().unwrap();
    // Rust note: Clone added for Rust's benefit. Not in the original.
    let loc = val.as_location().clone();
    state.loc_is_deallocand(&loc)
}

fn expr_binary_decide(expr: &AstExpr, state: &mut State) -> bool {
    let root = ast_expr_eval(ast_expr_binary_e1(expr), state).unwrap();
    let last = ast_expr_eval(ast_expr_binary_e2(expr), state).unwrap();
    // Note: `root` and `last` are leaked in the original.
    value_compare(&root, ast_expr_binary_op(expr), &last)
}

fn value_compare(v1: &Value, op: AstBinaryOp, v2: &Value) -> bool {
    match op {
        AstBinaryOp::Eq => Value::equal(v1, v2),
        AstBinaryOp::Ne => !value_compare(v1, AstBinaryOp::Eq, v2),
        _ => panic!(),
    }
}

pub fn ast_expr_eval(expr: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    match &expr.kind {
        AstExprKind::Constant(constant) => expr_constant_eval(constant, state),
        AstExprKind::StringLiteral(literal) => expr_literal_eval(literal, state),
        AstExprKind::Identifier(identifier) => expr_identifier_eval(identifier, state),
        AstExprKind::Unary(unary) => expr_unary_eval(unary, state),
        AstExprKind::StructMember(structmember) => expr_structmember_eval(structmember, state),
        AstExprKind::Call(call) => expr_call_eval(call, state),
        AstExprKind::Assignment(assign) => expr_assign_eval(assign, state),
        AstExprKind::IncDec(incdec) => expr_incdec_eval(incdec, state),
        AstExprKind::Binary(binary) => expr_binary_eval(binary, state),
        AstExprKind::ArbArg => arbarg_eval(state),
        AstExprKind::Bracketed(_)
        | AstExprKind::Iteration
        | AstExprKind::IsDeallocand(_)
        | AstExprKind::IsDereferencable(_)
        | AstExprKind::Allocation(_) => panic!("unsupported expression for eval: `{expr}'"),
    }
}

fn expr_literal_eval(literal: &str, state: &mut State) -> Result<Box<Value>> {
    Ok(state.static_init(literal))
}

fn expr_constant_eval(constant: &ConstantExpr, _state: &mut State) -> Result<Box<Value>> {
    Ok(Value::new_int(constant.constant))
}

fn expr_identifier_eval(id: &str, state: &mut State) -> Result<Box<Value>> {
    if let Ok(res) = hack_identifier_builtin_eval(id, state) {
        return Ok(res);
    }

    /* XXX */
    if id.starts_with('#') {
        return Ok(Value::new_literal(id));
    }
    // Note: Original does not null-check `obj` when there is not an error.
    let obj = state.get_object(id)?.unwrap();
    let Some(val) = obj.as_value() else {
        return Err(Error::new(format!(
            "undefined memory access: `{id}' has no value",
        )));
    };
    Ok(Value::copy(val))
}

fn hack_identifier_builtin_eval(id: &str, state: &State) -> Result<Box<Value>> {
    if state.get_vconst(id).is_some() || id.starts_with("ptr:") {
        return Ok(Value::new_sync(AstExpr::new_identifier(id.to_string())));
    }
    Err(Error::new("not built-in".to_string()))
}

fn expr_unary_eval(unary: &UnaryExpr, state: &mut State) -> Result<Box<Value>> {
    match unary.op {
        AstUnaryOp::Dereference => dereference_eval(&unary.arg, state),
        AstUnaryOp::Address => address_eval(&unary.arg, state),
        // XXX: hack because we stmt_exec pre as a preproces to verify
        // constructors, this breaks any preconditions like: pre: !(p == 0)
        AstUnaryOp::Bang => Ok(Value::new_literal("hack")),
        _ => panic!(),
    }
}

fn dereference_eval(operand: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    let binary = expr_to_binary(operand);
    binary_deref_eval(&binary, state)
}

fn expr_to_binary(expr: &AstExpr) -> Box<AstExpr> {
    match &expr.kind {
        AstExprKind::Binary(_) => ast_expr_copy(expr),
        _ => AstExpr::new_binary(
            ast_expr_copy(expr),
            AstBinaryOp::Addition,
            AstExpr::new_constant(0),
        ),
    }
}

fn binary_deref_eval(expr: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    let arr = ast_expr_eval(ast_expr_binary_e1(expr), state)?;
    // Note: Original seems to leak `arr`.
    let Some(deref_obj) = state.deref(&arr, ast_expr_binary_e2(expr))? else {
        return Err(Error::new(format!(
            "undefined indirection: *({expr}) has no value"
        )));
    };
    let Some(v) = deref_obj.as_value() else {
        return Err(Error::new(format!(
            "undefined indirection: *({expr}) has no value"
        )));
    };
    Ok(Value::copy(v))
}

fn address_eval(operand: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    let id = ast_expr_as_identifier(operand);
    Ok(state.get_loc(id))
}

fn expr_structmember_eval(expr: &StructMemberExpr, s: &mut State) -> Result<Box<Value>> {
    let StructMemberExpr { root, member } = expr;
    let res_val = ast_expr_eval(root, s)?;
    let Some(member) = res_val.struct_member(member) else {
        return Err(Error::new(format!("`{root}' has no field `{member}'")));
    };
    let Some(obj_value) = member.as_value() else {
        // Note: Original would return null if obj_value is null, but almost nobody downstream
        // handles it.
        panic!();
    };
    Ok(Value::copy(obj_value))
}

fn expr_call_eval(call: &CallExpr, state: &mut State) -> Result<Box<Value>> {
    let CallExpr { fun, args } = call;
    let name = ast_expr_as_identifier(fun);
    let Some(f) = (*state).ext().get_func(name) else {
        return Err(Error::new(format!("`{name}' not found")));
    };
    let params = f.params();

    if args.len() != params.len() {
        return Err(Error::new(format!(
            "`{name}' given {} arguments instead of {}",
            args.len(),
            params.len()
        )));
    }

    let args = prepare_arguments(args, params, state);
    state.push_frame(f.name.clone(), &f.abstract_, f.rtype(), true);
    prepare_parameters(params, args, name, state)?;

    /* XXX: pass copy so we don't observe */
    call_setupverify(f, &mut state.clone())
        .map_err(|err| err.wrap(format!("`{name}' precondition failure\n\t")))?;
    let v = call_absexec(call, state).map_err(|err| err.wrap("\n\t".to_string()))?;
    state.pop_frame();
    pf_augment(&v, call, state)
}

fn call_absexec(call: &CallExpr, s: &mut State) -> Result<Box<Value>> {
    let name = ast_expr_as_identifier(&call.fun);
    let Some(f) = s.ext().get_func(name) else {
        return Err(Error::new(format!("function `{name}' not found")));
    };
    // Note: In the original, this checked for `ast_function_absexec` returning a result with
    // null value, and in that case called `call_arbitraryresult`. However, this can't happen.
    if let Some(v) = ast_function_absexec(f, s)? {
        return Ok(v);
    }
    call_arbitraryresult(call, f, s)
}

fn call_setupverify(f: &AstFunction, arg_state: &mut State) -> Result<()> {
    let fname = f.name();
    let mut param_state = State::new(
        fname.to_string(),
        &f.abstract_,
        f.rtype(),
        true,
        arg_state.ext(),
    );
    ast_function_initparams(f, &mut param_state)?;
    let params = f.params();
    for p in params {
        let id = &p.name;
        // Note: `param` and `arg` are deliberately leaked in the original to avoid double-freeing
        // the variable's location.
        let param = param_state.get_loc(id);
        let arg = arg_state.get_loc(id);
        verify_paramspec(&param, &arg, &mut param_state, arg_state)
            .map_err(|err| err.wrap(format!("parameter `{id}' of `{fname}' ")))?;
    }
    // Note: Original leaks the state.
    Ok(())
}

fn verify_paramspec(
    param: &Value,
    arg: &Value,
    param_state: &mut State,
    arg_state: &mut State,
) -> Result<()> {
    if !param_state.is_lval(param) {
        return Ok(());
    }
    if !arg_state.is_lval(arg) {
        return Err(Error::new("must be lvalue".to_string()));
    }
    if param_state.is_alloc(param) && !arg_state.is_alloc(arg) {
        return Err(Error::new("must be heap allocated".to_string()));
    }
    let param_obj = param_state.get_mut(param.as_location(), false)?.unwrap();
    let arg_obj = arg_state.get_mut(arg.as_location(), false)?.unwrap();
    if !param_obj.has_value() {
        return Ok(());
    }
    if !arg_obj.has_value() {
        return Err(Error::new("must be rvalue".to_string()));
    }
    // Rust note: Unlike the original, we copy the values to satisfy Rust alias analysis.
    let param_val = param_obj.as_value().unwrap().clone();
    let arg_val = arg_obj.as_value().unwrap().clone();
    verify_paramspec(&param_val, &arg_val, param_state, arg_state)
}

fn pf_augment(v: &Value, call: &CallExpr, state: &mut State) -> Result<Box<Value>> {
    if !v.is_struct() {
        return Ok(Value::copy(v));
    }
    // Note: Original leaked a result and a value here.
    let res_val = call_pf_reduce(call, state)?;
    Ok(v.pf_augment(res_val.as_sync()))
}

fn call_arbitraryresult(
    _call: &CallExpr,
    f: &AstFunction,
    state: &mut State,
) -> Result<Box<Value>> {
    let res = call_to_computed_value(f, state)?;
    Ok(res)
}

fn call_to_computed_value(f: &AstFunction, s: &mut State) -> Result<Box<Value>> {
    let root = f.name();
    let uncomputed_params = f.params();
    let nparams = uncomputed_params.len();
    let mut computed_params = Vec::with_capacity(nparams);
    for p in uncomputed_params {
        let param = AstExpr::new_identifier(p.name.clone());
        // Note: The original leaked a result here.
        let v = ast_expr_eval(&param, s)?;
        computed_params.push(if v.is_location() {
            AstExpr::new_identifier(v.to_string())
        } else {
            v.to_expr()
        });
    }
    Ok(Value::new_sync(AstExpr::new_call(
        AstExpr::new_identifier(root.to_string()),
        computed_params,
    )))
}

pub fn prepare_arguments(
    args: &[Box<AstExpr>],
    params: &[Box<AstVariable>],
    state: &mut State,
) -> Vec<Result<Box<Value>>> {
    assert_eq!(args.len(), params.len());
    args.iter().map(|arg| ast_expr_eval(arg, state)).collect()
}

/// Allocates arguments in call expression and assigns them to their respective parameters.
pub fn prepare_parameters(
    params: &[Box<AstVariable>],
    args: Vec<Result<Box<Value>>>,
    // Note: Argument `fname` is unused after removing some unreachable code.
    _fname: &str,
    state: &mut State,
) -> Result<()> {
    assert_eq!(params.len(), args.len());
    for (param, res) in params.iter().zip(args) {
        state.declare(param, true);

        let arg = res?;
        let name = AstExpr::new_identifier(param.name.clone());
        let lval_lval = ast_expr_lvalue(&name, state)?;
        let obj = lval_lval.obj.unwrap();
        // Note: Original does not null-check `obj`.
        // Note: I think the arg is copied needlessly in the original, and one is leaked.
        obj.assign(Some(arg));
    }
    Ok(())
}

fn expr_assign_eval(assign: &AssignmentExpr, state: &mut State) -> Result<Box<Value>> {
    let AssignmentExpr { lval, rval } = assign;
    let rval_val = ast_expr_eval(rval, state)?;
    let lval_lval = ast_expr_lvalue(lval, state)?;
    let Some(obj) = lval_lval.obj else {
        return Err(Error::new(format!(
            "undefined indirection: {lval} is not an lvalue"
        )));
    };
    obj.assign(Some(Value::copy(&rval_val)));
    Ok(rval_val)
}

fn expr_incdec_eval(incdec: &IncDecExpr, state: &mut State) -> Result<Box<Value>> {
    let assign = ast_expr_incdec_to_assignment(incdec);
    if incdec.pre {
        expr_assign_eval(&assign, state)
    } else {
        let res = ast_expr_eval(&incdec.operand, state);
        // Note: The original also ignored errors here.
        let _ = expr_assign_eval(&assign, state);
        res
    }
}

fn expr_binary_eval(binary: &BinaryExpr, state: &mut State) -> Result<Box<Value>> {
    let BinaryExpr { op, e1, e2 } = binary;
    // Note: Both values are leaked in the original.
    let v1 = ast_expr_eval(e1, state)?;
    let v2 = ast_expr_eval(e2, state)?;
    let result = Value::new_sync(AstExpr::new_binary(v1.to_expr(), *op, v2.to_expr()));
    Ok(result)
}

fn arbarg_eval(state: &mut State) -> Result<Box<Value>> {
    Ok(state.vconst(
        &AstType::new_ptr(AstType::new(AstTypeBase::Void, 0)),
        None,
        false,
    ))
}

pub fn ast_expr_abseval(expr: &AstExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    match &expr.kind {
        AstExprKind::Assignment(assign) => assign_absexec(assign, state),
        AstExprKind::IsDereferencable(_) => isdereferencable_absexec(expr, state),
        AstExprKind::Allocation(alloc) => alloc_absexec(alloc, state),
        AstExprKind::Identifier(_)
        | AstExprKind::Constant(_)
        | AstExprKind::Unary(_)
        | AstExprKind::Call(_)
        | AstExprKind::StructMember(_)
        | AstExprKind::ArbArg => Ok(Some(ast_expr_eval(expr, state)?)),
        _ => panic!(),
    }
}

fn alloc_absexec(alloc: &AllocExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    match alloc.kind {
        AstAllocKind::Alloc => Ok(Some(state.alloc())),
        AstAllocKind::Dealloc => dealloc_process(alloc, state),
        AstAllocKind::Clump => Ok(Some(state.clump())),
    }
}

fn dealloc_process(alloc: &AllocExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    let val = ast_expr_eval(&alloc.arg, state)?;
    state.dealloc(&val)?;
    Ok(None)
}

fn assign_absexec(assign: &AssignmentExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    let AssignmentExpr { lval, rval } = assign;
    let Some(val) = ast_expr_abseval(rval, state)? else {
        debug_assert!(false);
        return Err(Error::new("undefined indirection (rvalue)".to_string()));
    };
    let lval_res = ast_expr_lvalue(lval, state)?;
    let Some(obj) = lval_res.obj else {
        return Err(Error::new("undefined indirection (lvalue)".to_string()));
    };
    obj.assign(Some(Value::copy(&val)));
    Ok(Some(val))
}

fn isdereferencable_absexec(expr: &AstExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    // Note: In the original it's unclear why it's safe for `state.props` to assume ownership of
    // `expr`. The way we solved the puzzle, copying is correct.
    let p = state.props();
    p.install(ast_expr_copy(expr));
    Ok(None)
}

pub fn ast_expr_assume(expr: &AstExpr, state: &mut State) -> Result<Preresult> {
    reduce_assume(expr, true, state)
}

fn reduce_assume(expr: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
    match &expr.kind {
        AstExprKind::Identifier(_) => identifier_assume(expr, value, s),
        AstExprKind::Unary(unary) => {
            assert_eq!(unary.op, AstUnaryOp::Bang);
            reduce_assume(&unary.arg, !value, s)
        }
        AstExprKind::Bracketed(inner) => reduce_assume(inner, value, s),
        AstExprKind::Call(_) | AstExprKind::StructMember(_) => {
            ast_expr_pf_reduce_assume(expr, value, s)
        }
        AstExprKind::Binary(binary) => binary_assume(binary, value, s),
        _ => {
            panic!();
        }
    }
}

fn identifier_assume(expr: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
    let mut s_copy = s.clone();
    let res_val = ast_expr_eval(expr, &mut s_copy).unwrap();
    irreducible_assume(&res_val.into_sync(), value, s)
}

fn ast_expr_pf_reduce_assume(expr: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
    // TODO: user errors
    let res_val = ast_expr_pf_reduce(expr, s).unwrap();
    irreducible_assume(&res_val.into_sync(), value, s)
}

pub fn ast_expr_pf_reduce(e: &AstExpr, s: &mut State) -> Result<Box<Value>> {
    match &e.kind {
        AstExprKind::Constant(_) | AstExprKind::StringLiteral(_) | AstExprKind::Identifier(_) => {
            ast_expr_eval(e, s)
        }
        AstExprKind::Unary(_) => unary_pf_reduce(e, s),
        AstExprKind::Binary(binary) => binary_pf_reduce(binary, s),
        AstExprKind::Call(call) => call_pf_reduce(call, s),
        AstExprKind::StructMember(sm) => structmember_pf_reduce(sm, s),
        AstExprKind::Bracketed(inner) => ast_expr_pf_reduce(inner, s),
        _ => panic!(),
    }
}

fn unary_pf_reduce(e: &AstExpr, s: &mut State) -> Result<Box<Value>> {
    let res_val = ast_expr_pf_reduce(ast_expr_unary_operand(e), s)?;
    Ok(Value::new_sync(AstExpr::new_unary(
        res_val.into_sync(),
        ast_expr_unary_op(e),
    )))
}

fn binary_pf_reduce(binary: &BinaryExpr, s: &mut State) -> Result<Box<Value>> {
    let BinaryExpr { e1, op, e2 } = binary;
    let v1 = ast_expr_pf_reduce(e1, s)?;
    let v2 = ast_expr_pf_reduce(e2, s)?;
    // Note: Original leaked v1 and v2.
    Ok(Value::new_sync(AstExpr::new_binary(
        v1.to_expr(),
        *op,
        v2.to_expr(),
    )))
}

fn call_pf_reduce(call: &CallExpr, s: &mut State) -> Result<Box<Value>> {
    let CallExpr {
        fun,
        args: unreduced_args,
    } = call;
    let root = ast_expr_as_identifier(fun);
    let mut reduced_args = Vec::with_capacity(unreduced_args.len());
    for arg in unreduced_args {
        // Note: Original leaked a result and a value here.
        let val = ast_expr_pf_reduce(arg, s)?;
        reduced_args.push(val.to_expr());
    }
    Ok(Value::new_sync(AstExpr::new_call(
        AstExpr::new_identifier(root.to_string()),
        reduced_args,
    )))
}

fn structmember_pf_reduce(sm: &StructMemberExpr, s: &mut State) -> Result<Box<Value>> {
    let StructMemberExpr { root, member } = sm;
    let v = ast_expr_pf_reduce(root, s)?;
    if v.is_struct() {
        let obj = v.struct_member(member).unwrap();
        let obj_value = obj.as_value().unwrap();
        return Ok(Value::copy(obj_value));
    }
    assert!(v.is_sync());
    Ok(Value::new_sync(AstExpr::new_member(
        v.into_sync(),
        member.to_string(),
    )))
}

fn irreducible_assume(e: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
    let prop = ast_expr_inverted_copy(e, !value);
    irreducible_assume_actual(&prop, s)
}

fn irreducible_assume_actual(e: &AstExpr, s: &mut State) -> Result<Preresult> {
    let p = s.props();
    if p.contradicts(e) {
        return Ok(Preresult {
            is_contradiction: true,
        });
    }
    p.install(ast_expr_copy(e));
    Ok(Preresult {
        is_contradiction: false,
    })
}

fn binary_assume(b: &BinaryExpr, value: bool, s: &mut State) -> Result<Preresult> {
    let v1 = ast_expr_pf_reduce(&b.e1, s).unwrap();
    let v2 = ast_expr_pf_reduce(&b.e2, s).unwrap();
    // Note: Original leaked the expression.
    let expr = AstExpr::new_binary(v1.to_expr(), b.op, v2.to_expr());
    irreducible_assume(&expr, value, s)
}
