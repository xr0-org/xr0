use std::ffi::CString;
use std::ptr;

use crate::ast::*;
use crate::parser::env::Env;
use crate::util::dynamic_str;

type BoxedAst = *mut Ast;
type BoxedBlock = *mut AstBlock;
type BoxedExpr = *mut AstExpr;
type BoxedFunction = *mut AstFunction;
type BoxedStmt = *mut AstStmt;
type BoxedType = *mut AstType;
type BoxedVariable = *mut AstVariable;
type BoxedVariableArray = *mut AstVariableArr;
type BoxedExternDecl = *mut AstExternDecl;
type BoxedCStr = *mut libc::c_char;

pub struct Declaration {
    pub name: *mut libc::c_char,
    pub t: *mut AstType,
}

pub struct DirectFunctionDeclarator {
    pub name: *mut libc::c_char,
    pub n: libc::c_int,
    pub param: *mut *mut AstVariable,
}

pub struct FunctionDeclarator {
    pub ptr_valence: libc::c_int,
    pub decl: DirectFunctionDeclarator,
}

pub struct Declarator {
    pub ptr_valence: libc::c_int,
    pub name: *mut libc::c_char,
}

pub struct ExprArray {
    pub n: libc::c_int,
    pub expr: *mut *mut AstExpr,
}

pub struct StmtArray {
    pub n: libc::c_int,
    pub stmt: *mut *mut AstStmt,
}

struct BlockStatement {
    r#abstract: *mut AstBlock,
    body: *mut AstBlock,
}

enum PostfixOp {
    ArrayAccess(*mut AstExpr),
    Call(libc::c_int, *mut *mut AstExpr),
    Dot(*mut libc::c_char),
    Arrow(*mut libc::c_char),
    Inc,
    Dec,
}

/* XXX */
unsafe fn strip_quotes(s: *const libc::c_char) -> BoxedCStr {
    assert_eq!(*s, b'"' as libc::c_char);
    let len = libc::strlen(s) - 2 + 1;
    let t = libc::malloc(len) as *mut libc::c_char;
    libc::memcpy(
        t as *mut libc::c_void,
        s.offset(1) as *const libc::c_void,
        len - 1,
    );
    *t.add(len - 1) = 0;
    t
}

unsafe fn expr_array_from_vec(v: Vec<*mut AstExpr>) -> ExprArray {
    let size_bytes = std::mem::size_of::<*mut AstExpr>() * v.len();
    let expr = libc::malloc(size_bytes) as *mut *mut AstExpr;
    assert!(!expr.is_null());
    libc::memmove(
        expr as *mut libc::c_void,
        v.as_ptr() as *const libc::c_void,
        size_bytes,
    );
    ExprArray {
        n: v.len() as libc::c_int,
        expr,
    }
}

unsafe fn stmt_array_from_vec(v: Vec<*mut AstStmt>) -> StmtArray {
    let size_bytes = std::mem::size_of::<*mut AstStmt>() * v.len();
    let stmt = libc::malloc(size_bytes) as *mut *mut AstStmt;
    assert!(!stmt.is_null());
    libc::memmove(
        stmt as *mut libc::c_void,
        v.as_ptr() as *const libc::c_void,
        size_bytes,
    );
    StmtArray {
        n: v.len() as libc::c_int,
        stmt,
    }
}

unsafe fn variable_array_from_vec(vars: Vec<*mut AstVariable>) -> *mut AstVariableArr {
    let list = ast_variable_arr_create();
    for v in vars {
        ast_variable_arr_append(list, v);
    }
    list
}

unsafe fn variable_array_from_decl_vec(decls: Vec<Declaration>) -> *mut AstVariableArr {
    let list = ast_variable_arr_create();
    for decl in decls {
        ast_variable_arr_append(list, ast_variable_create(decl.name, decl.t));
    }
    list
}

unsafe fn ast_from_vec(v: Vec<*mut AstExternDecl>) -> *mut Ast {
    let mut root = ast_create(v[0]);
    for &decl in &v[1..] {
        root = ast_append(root, decl);
    }
    root
}

peg::parser! {
pub grammar c_parser(env: &Env) for str {
    // A few bits cribbed from <https://github.com/vickenty/lang-c/blob/master/grammar.rustpeg>

    // Lists of elements.
    rule list0<T>(ex: rule<T>) -> Vec<T> = e:ex() ** _ { e }
    rule list1<T>(ex: rule<T>) -> Vec<T> = e:ex() ++ _ { e }
    rule cs0<T>(ex: rule<T>) -> Vec<T> = e:ex() ** (_ "," _) { e }
    rule cs1<T>(ex: rule<T>) -> Vec<T> = e:ex() ++ (_ "," _) { e }

    // Whitespace
    rule _() = quiet!{(newline() / [ ' ' | '\t'])*}
    rule newline() =
        "\r"? "\n" d:$(directive()?) {
            unsafe {
                env.newline();
                if !d.is_empty() {
                    env.directive(d)
                }
            }
        }
    rule directive() = "#" [^ '\n']*
    rule start_directive() = d:$(directive()) { unsafe { env.directive(d); } }

    // Keywords
    rule end_of_token() = !['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']
    rule K<T>(t: rule<T>) -> T = quiet! { e:t() end_of_token() { e } }

    // Identifiers
    rule identifier() -> BoxedCStr =
        n:quiet! { $(['_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) } {?
            if !env.reserved.contains(n) {
                let cstr = CString::new(n.to_string()).unwrap();
                Ok(unsafe { dynamic_str(cstr.as_ptr()) })
            } else {
                Err("identifier")
            }
        } /
        expected!("identifier")

    // Constants
    rule dec() = ['0'..='9']
    rule oct() = ['0'..='7']
    rule hex() = ['0'..='9' | 'a'..='f' | 'A'..='F']

    pub rule constant() -> BoxedExpr =
        numeric_constant() / character_constant()

    rule numeric_constant() -> BoxedExpr =
        n:quiet! { integer_number() } { unsafe { ast_expr_constant_create(n) } } /
        expected!("number")

    rule integer_number() -> libc::c_int =
        n:$(['1'..='9'] dec()* / "0") end_of_token() {
            unsafe {
                let cstr = CString::new(n.to_string()).unwrap();
                parse_int(cstr.as_ptr() as *mut libc::c_char)
            }
        }

    rule character_constant() -> BoxedExpr =
        c:quiet! { $("'" character()+ "'") } {
            unsafe {
                let cstr = CString::new(c.to_string()).unwrap();
                ast_expr_constant_create_char(parse_char(cstr.as_ptr() as *mut libc::c_char))
            }
        } /
        expected!("character")

    rule character() = [^ '\'' | '\\' | '\n'] / "\\" [^ '\n']

    // String literals
    rule string_literal() -> BoxedExpr =
        a:$("\"" string_char()* "\"") {
            unsafe {
                let cstr = CString::new(a.to_string()).unwrap();
                ast_expr_literal_create(strip_quotes(cstr.as_ptr()))
            }
        }

    rule string_char() = [^ '"' | '\\' | '\n'] / "\\" [^ '\n']

    // 6.5.1 Primary expression
    rule primary_expression() -> BoxedExpr =
        a:identifier() {
            unsafe { ast_expr_identifier_create(a) }
        } /
        "$" {
            unsafe { ast_expr_arbarg_create() }
        } /
        constant() /
        string_literal() /
        "(" _ a:expression() _ ")" { a }

    // 6.5.2 Postfix operators
    rule postfix_expression() -> BoxedExpr =
        a:primary_expression() ops:postfix_op()* {
            unsafe {
                let mut a = a;
                for op in ops {
                    a = match op {
                        PostfixOp::ArrayAccess(i) =>
                            ast_expr_unary_create(
                                ast_expr_binary_create(a, AstBinaryOp::Addition, i),
                                AstUnaryOp::Dereference,
                            ),
                        PostfixOp::Call(argc, argv) =>
                            ast_expr_call_create(a, argc, argv),
                        PostfixOp::Dot(name) =>
                            ast_expr_member_create(a, name),
                        PostfixOp::Arrow(name) =>
                            ast_expr_member_create(
                                ast_expr_unary_create(
                                    ast_expr_binary_create(
                                        a,
                                        AstBinaryOp::Addition,
                                        ast_expr_constant_create(0)
                                    ),
                                    AstUnaryOp::Dereference
                                ),
                                name,
                            ),
                        PostfixOp::Inc => ast_expr_incdec_create(a, true, false),
                        PostfixOp::Dec => ast_expr_incdec_create(a, false, false),
                    };
                }
                a
            }
        } /
        allocation_expression()

    rule postfix_op() -> PostfixOp =
        _ "[" _ i:expression() _ "]" { PostfixOp::ArrayAccess(i) } /
        _ "(" _ ")" { PostfixOp::Call(0, ptr::null_mut()) } /
        _ "(" _ args:argument_expression_list() _ ")" { PostfixOp::Call(args.n, args.expr) } /
        _ "." _ m:identifier() { PostfixOp::Dot(m) } /
        _ "->" _ m:identifier() { PostfixOp::Arrow(m) } /
        _ "++" { PostfixOp::Inc } /
        _ "--" { PostfixOp::Dec }

    rule allocation_expression() -> BoxedExpr =
        ".malloc" _ "(" a:expression() ")" { unsafe { ast_expr_alloc_create(a) } } /
        ".free" _ "(" a:expression() ")" { unsafe { ast_expr_dealloc_create(a) } } /
        ".clump" _ "(" a:expression() ")" { unsafe { ast_expr_clump_create(a) } }

    rule argument_expression_list() -> ExprArray =
        args:cs1(<assignment_expression()>) { unsafe { expr_array_from_vec(args) } }

    rule isdeallocand_expression() -> BoxedExpr =
        postfix_expression() /
        "@" _ a:isdeallocand_expression() { unsafe { ast_expr_isdeallocand_create(a) } } /
        "$" _ a:isdeallocand_expression() { unsafe { ast_expr_isdereferencable_create(a) } }

    // 6.5.3 Unary operators
    rule unary_expression() -> BoxedExpr =
        isdeallocand_expression() /
        "++" _ a:unary_expression() { unsafe { ast_expr_incdec_create(a, true, true) } } /
        "--" _ a:unary_expression() { unsafe { ast_expr_incdec_create(a, false, true) } } /
        op:unary_operator() _ e:cast_expression() { unsafe { ast_expr_unary_create(e, op) } } /
        "sizeof" _ "(" _ ty:type_name() _ ")" { unsafe { ast_expr_constant_create(1) /* XXX */ } }

    rule unary_operator() -> AstUnaryOp =
        "&" !"&" { AstUnaryOp::Address } /
        "*" { AstUnaryOp::Dereference } /
        "+" { AstUnaryOp::Positive } /
        "-" { AstUnaryOp::Negative } /
        "~" { AstUnaryOp::OnesComplement } /
        "!" { AstUnaryOp::Bang }

    // 6.5.4 Cast expressions
    rule cast_expression() -> BoxedExpr = unary_expression()

    // 6.5.5 -- 6.5.14 Binary operators
    rule binary_expression() -> BoxedExpr = precedence! {
        x:(@) _ "||" _ y:@ { x }
        --
        x:(@) _ "&&" _ y:@ { x }
        --
        x:(@) _ "==" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Eq, y) } }
        x:(@) _ "!=" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Ne, y) } }
        --
        x:(@) _ "<" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Lt, y) } }
        x:(@) _ ">" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Gt, y) } }
        x:(@) _ "<=" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Le, y) } }
        x:(@) _ ">=" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Ge, y) } }
        --
        x:(@) _ "+" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Addition, y) } }
        x:(@) _ "-" _ y:@ { unsafe { ast_expr_binary_create(x, AstBinaryOp::Subtraction, y) } }
        --
        x:(@) _ "*" _ y:@ { x }
        x:(@) _ "/" _ y:@ { x }
        x:(@) _ "%" _ y:@ { x }
        --
        e:cast_expression() { e }
    }

    rule conditional_expression() -> BoxedExpr = binary_expression()

    // 6.5.16 Assignment operators
    rule assignment_expression() -> BoxedExpr =
        a:unary_expression() _ assignment_operator() _ b:assignment_expression() {
            unsafe { ast_expr_assignment_create(a, b) }
        } /
        conditional_expression()

    rule assignment_operator() =
        "=" / ">>=" / "<<=" / "+=" / "-=" / "*=" / "/=" / "%=" / "&=" / "^=" / "|="

    pub rule expression() -> BoxedExpr = assignment_expression();

    // 6.6 Constant expressions
    rule constant_expression() -> BoxedExpr = conditional_expression()

    // 6.7 Declarations
    rule declaration() -> Declaration =
        t:declaration_specifiers() _ ";" {
            unsafe {
                Declaration { name: ast_type_struct_tag(t), t }
            }
        } /
        t:declaration_specifiers() _ v:declarator() _ ";" {
            unsafe {
                let is_typedef = (*t).mod_0 & MOD_TYPEDEF as libc::c_int != 0;

                let mut t = t;
                for _ in 0..v.ptr_valence {
                    t = ast_type_create_ptr(t);
                }
                if is_typedef {
                    env.add_typename(v.name);
                }
                Declaration { name: v.name, t }
            }
        }

    rule declaration_modifier() -> AstTypeModifier =
        storage_class_specifier() /
        type_qualifier()

    rule declaration_specifiers() -> BoxedType =
        type_specifier() /
        type_specifier() _ t:declaration_specifiers() { t } /
        m:declaration_modifier() _ s:declaration_specifiers() {
            unsafe {
                ast_type_mod_or(s, m);
            }
            s
        }

    rule storage_class_specifier() -> AstTypeModifier =
        K(<"typedef">) {
            // XXX TODO
        /* XXX: typedef is not modelled as a modifier, so our only
         * concern is to prepare the lexer to return the next string it
         * sees as a TYPE_NAME, so that in the type_specifier beneath
         * the fact that this is a typedef will be captured */
            //env.installclass = IC_TYPE;
            // XXX probably handle this in the enclosing rule instead
            MOD_TYPEDEF
        } /
        K(<"extern">) { MOD_EXTERN } /
        K(<"static">) { MOD_STATIC } /
        K(<"auto">) { MOD_AUTO } /
        K(<"register">) { MOD_REGISTER }

    rule type_specifier() -> BoxedType =
        K(<"void">) { unsafe { ast_type_create(AstTypeBase::Void, 0) } } /
        K(<"char">) { unsafe { ast_type_create(AstTypeBase::Char, 0) } } /
        K(<"int">) { unsafe { ast_type_create(AstTypeBase::Int, 0) } } /
        struct_or_union_specifier() /
        t:typedef_name() { unsafe { ast_type_create_userdef(dynamic_str(t)) } }

    rule typedef_name() -> BoxedCStr = i:identifier() {?
        unsafe {
            if env.is_typename(i) {
                Ok(i)
            } else {
                Err("<unused>")
            }
        }
    }

    // note: unions are unsupported in the original
    rule struct_or_union_specifier() -> BoxedType =
        "struct" _ tag:identifier() _ "{" _ fields:struct_declaration_list() _ "}" {
            unsafe { ast_type_create_struct(tag, fields) }
        } /
        "struct" _ "{" _ fields:struct_declaration_list() _ "}" {
            unsafe { ast_type_create_struct_anonym(fields) }
        } /
        "struct" _ tag:identifier() {
            unsafe { ast_type_create_struct_partial(tag) }
        }

    rule struct_declaration_list() -> BoxedVariableArray =
        decls:list1(<struct_declaration()>) { unsafe { variable_array_from_vec(decls) } }

    rule struct_declaration() -> BoxedVariable =
        d:declaration() {
            unsafe { ast_variable_create(d.name, d.t) }
        }

    rule type_qualifier() -> AstTypeModifier =
        K(<"const">) { MOD_CONST } /
        K(<"volatile">) { MOD_VOLATILE }

    rule function_declarator() -> FunctionDeclarator =
        p:pointer() _ d:direct_function_declarator() {
            FunctionDeclarator { ptr_valence: p, decl: d }
        } /
        d:direct_function_declarator() {
            FunctionDeclarator { ptr_valence: 0, decl: d }
        }

    rule direct_function_declarator() -> DirectFunctionDeclarator =
        name:identifier() _ "(" _ params:parameter_type_list() _ ")" {
            unsafe {
                DirectFunctionDeclarator { name, n: ast_variable_arr_n(params), param: ast_variable_arr_v(params) }
            }
        } /
        name:identifier() _ "(" _ ")" {
            DirectFunctionDeclarator { name, n: 0, param: ptr::null_mut() }
        }

    rule declarator() -> Declarator =
        p:pointer() _ name:direct_declarator() { Declarator { ptr_valence: p, name } } /
        name:direct_declarator() { Declarator { ptr_valence: 0, name } } /
        p:pointer() { Declarator { ptr_valence: p, name: ptr::null_mut() } }

    rule direct_declarator() -> BoxedCStr =
        // Note: declarator postfix syntax is ignored in the original
        name:identifier() direct_declarator_postfix()* { name }

    rule direct_declarator_postfix() =
        _ "[" _ constant_expression() _ "]" /
        _ "(" _ parameter_type_list() _ ")"

    rule pointer() -> libc::c_int =
        s:$("*"+) { s.len() as libc::c_int }

    rule parameter_type_list() -> BoxedVariableArray = parameter_list()

    rule parameter_list() -> BoxedVariableArray =
        decls:cs1(<parameter_declaration()>) { unsafe { variable_array_from_vec(decls) } }

    rule parameter_declaration() -> BoxedVariable =
        t:declaration_specifiers() _ decl:declarator() {
            unsafe {
                let mut t = t;
                for _ in 0..decl.ptr_valence {
                    t = ast_type_create_ptr(t);
                }
                ast_variable_create(decl.name, t)
            }
        } /
        t:declaration_specifiers() {
            unsafe { ast_variable_create(dynamic_str(&(0 as libc::c_char)), t) }
        }

    rule type_name() -> BoxedType =
        t:specifier_qualifier_list() _ abstract_declarator() { t } /
        specifier_qualifier_list()

    rule abstract_declarator() -> libc::c_int = pointer()

    rule statement() -> BoxedStmt =
        labelled_statement() /
        p:position!() block:compound_statement() {
            unsafe { ast_stmt_create_compound(env.lexloc(p), block) }
        } /
        expression_statement() /
        selection_statement() /
        iteration_statement() /
        jump_statement() /
        stmt:iteration_effect_statement() {
            unsafe { ast_stmt_create_iter_e(stmt) }
        } /
        p:position!() v:compound_verification_statement() {
            unsafe { ast_stmt_create_compound_v(env.lexloc(p), v) }
        }

    rule specifier_qualifier_list() -> BoxedType =
        q:type_specifier() _ specifier_qualifier_list() { q } /
        q:type_specifier() { q } /
        // Note: original is I guess UB? type confusion
        type_qualifier() _ t:specifier_qualifier_list() { t } /
        type_qualifier() {
            unsafe { ast_type_create(AstTypeBase::Int, 0) }
        }

    rule labelled_statement() -> BoxedStmt =
        p:position!() label:identifier() _ ":" _ s:statement() {
            unsafe { ast_stmt_create_labelled(env.lexloc(p), label, s) }
        } /
        p:position!() K(<"case">) _ constant_expression() _ ":" _ statement() {
            unsafe { ast_stmt_create_nop(env.lexloc(p)) }
        } /
        p:position!() K(<"default">) _ ":" _ statement() {
            unsafe { ast_stmt_create_nop(env.lexloc(p)) }
        }

    rule compound_statement() -> BoxedBlock =
        "{" _ b:block() _ "}" { b }

    rule block() -> BoxedBlock =
        d:declaration_list() _ s:statement_list() {
            unsafe { ast_block_create(ast_variable_arr_v(d), ast_variable_arr_n(d), s.stmt, s.n) }
        } /
        d:declaration_list() {
            unsafe { ast_block_create(ast_variable_arr_v(d), ast_variable_arr_n(d), ptr::null_mut(), 0) }
        } /
        s:statement_list() {
            unsafe { ast_block_create(ptr::null_mut(), 0, s.stmt, s.n) }
        } /
        /* empty */ {
            unsafe { ast_block_create(ptr::null_mut(), 0, ptr::null_mut(), 0) }
        }

    rule iteration_effect_statement() -> BoxedStmt =
        "." _ s:for_iteration_statement() { s }

    rule compound_verification_statement() -> BoxedBlock =
        "~" _ "[" _ "]" {
            unsafe { ast_block_create(ptr::null_mut(), 0, ptr::null_mut(), 0) }
        } /
        "~" _ "[" _ b:block() _ "]" {
            b
        }

    rule declaration_list() -> BoxedVariableArray =
        decls:list1(<declaration()>) { unsafe { variable_array_from_decl_vec(decls) } }

    rule statement_list() -> StmtArray =
        stmts:list1(<statement()>) { unsafe { stmt_array_from_vec(stmts) } }

    rule expression_statement() -> BoxedStmt =
        p:position!() ";" { unsafe { ast_stmt_create_nop(env.lexloc(p)) } } /
        p:position!() e:expression() _ ";" { unsafe { ast_stmt_create_expr(env.lexloc(p), e) } }

    rule selection_statement() -> BoxedStmt =
        p:position!() K(<"if">) _ "(" _ cond:expression() _ ")" _ then:statement() _ K(<"else">) _ alt:statement() {
            unsafe {
                let neg_cond = ast_expr_unary_create(ast_expr_copy(cond), AstUnaryOp::Bang);
                let else_stmt = ast_stmt_create_sel(env.lexloc(p), false, neg_cond, alt, ptr::null_mut());
                ast_stmt_create_sel(env.lexloc(p), false, cond, then, else_stmt)
            }
        } /
        p:position!() K(<"if">) _ "(" _ cond:expression() _ ")" _ then:statement() {
            unsafe { ast_stmt_create_sel(env.lexloc(p), false, cond, then, ptr::null_mut()) }
        } /
        p:position!() K(<"switch">) _ "(" _ v:expression() _ ")" _ cases:statement() {
            unsafe { ast_stmt_create_nop(env.lexloc(p)) }
        }

    rule optional_compound_verification() -> BoxedBlock =
        vs:compound_verification_statement()? {
            vs.unwrap_or_else(|| {
                unsafe { ast_block_create(ptr::null_mut(), 0, ptr::null_mut(), 0) }
            })
        }

    rule for_iteration_statement() -> BoxedStmt =
        p:position!()
        K(<"for">) _ "(" _ init:expression_statement() _ cond:expression_statement() _ iter:expression() _ ")" _
        verif:optional_compound_verification() _ body:statement() {
            unsafe { ast_stmt_create_iter(env.lexloc(p), init, cond, iter, verif, body) }
        }

    rule iteration_statement() -> BoxedStmt = for_iteration_statement()

    rule jump_statement() -> BoxedStmt =
        p:position!() K(<"return">) _ expr:expression() _ ";" {
            unsafe { ast_stmt_create_jump(env.lexloc(p), JUMP_RETURN, expr) }
        }

    rule block_statement() -> BlockStatement =
        ";" {
            BlockStatement { r#abstract: ptr::null_mut(), body: ptr::null_mut() }
        } /
        c:compound_statement() {
            BlockStatement { r#abstract: ptr::null_mut(), body: c }
        } /
        v:compound_verification_statement() _ ";" {
            BlockStatement { r#abstract: v, body: ptr::null_mut() }
        } /
        v:compound_verification_statement() _ c:compound_statement() {
            BlockStatement { r#abstract: v, body: c }
        }

    rule function_definition() -> BoxedFunction =
        K(<"axiom">) _ t:declaration_specifiers() _ decl:function_declarator() _ body:block_statement() {
            unsafe {
                let mut t = t;
                for _ in 0..decl.ptr_valence {
                    t = ast_type_create_ptr(t);
                }
                ast_function_create(
                    true,
                    t,
                    decl.decl.name,
                    decl.decl.n,
                    decl.decl.param,
                    if body.r#abstract.is_null() {
                        ast_block_create(ptr::null_mut(), 0, ptr::null_mut(), 0)
                    } else {
                        body.r#abstract
                    },
                    body.body
                )
            }
        } /
        t:declaration_specifiers() _ decl:function_declarator() _ body:block_statement() {
            unsafe {
                let mut t = t;
                for _ in 0..decl.ptr_valence {
                    t = ast_type_create_ptr(t);
                }
                ast_function_create(
                    false,
                    t,
                    decl.decl.name,
                    decl.decl.n,
                    decl.decl.param,
                    if body.r#abstract.is_null() {
                        ast_block_create(ptr::null_mut(), 0, ptr::null_mut(), 0)
                    } else {
                        body.r#abstract
                    },
                    body.body
                )
            }
        } /
        // Note: this was in the original. surely shouldn't be allowed, and implicit return type
        // should be int if it is allowed.
        decl:function_declarator() _ body:block_statement() {
            unsafe {
                ast_function_create(
                    false,
                    ast_type_create(AstTypeBase::Void, 0),
                    decl.decl.name,
                    decl.decl.n,
                    decl.decl.param,
                    if body.r#abstract.is_null() {
                        ast_block_create(ptr::null_mut(), 0, ptr::null_mut(), 0)
                    } else {
                        body.r#abstract
                    },
                    body.body
                )
            }
        }

    rule external_declaration() -> BoxedExternDecl =
        f:function_definition() { unsafe { ast_functiondecl_create(f) } } /
        d:declaration() { unsafe { ast_decl_create(d.name, d.t) } }

    pub rule translation_unit() -> BoxedAst =
        start_directive()? _ decl:list1(<external_declaration()>) _ { unsafe { ast_from_vec(decl) } }
}
}