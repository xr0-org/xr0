use crate::ast::*;
use crate::parser::env::Env;
use crate::util::{OwningCStr, SemiBox};

type BoxedFunction = *mut AstFunction<'static>;

pub struct Declaration {
    pub name: Option<OwningCStr>,
    pub t: Box<AstType>,
}

pub struct DirectFunctionDeclarator {
    pub name: OwningCStr,
    pub params: Vec<Box<AstVariable>>,
}

pub struct FunctionDeclarator {
    pub ptr_valence: libc::c_int,
    pub decl: DirectFunctionDeclarator,
}

pub struct Declarator {
    pub ptr_valence: libc::c_int,
    pub name: Option<OwningCStr>,
}

struct BlockStatement {
    abstract_: Option<Box<AstBlock>>,
    body: Option<Box<AstBlock>>,
}

enum PostfixOp {
    ArrayAccess(Box<AstExpr>),
    Call(Vec<Box<AstExpr>>),
    Dot(OwningCStr),
    Arrow(OwningCStr),
    Inc,
    Dec,
}

fn variable_array_from_decl_vec(decls: Vec<Declaration>) -> Vec<Box<AstVariable>> {
    decls
        .into_iter()
        .map(|decl| ast_variable_create(decl.name.unwrap(), decl.t))
        .collect()
}

fn ast_from_vec(decls: Vec<Box<AstExternDecl>>) -> Box<Ast> {
    Box::new(Ast { decls })
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
    rule newline() = "\r"? "\n" $(directive()?)
    rule directive() = "#" [^ '\n']*

    // Keywords
    rule end_of_token() = !['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']
    rule K<T>(t: rule<T>) -> T = quiet! { e:t() end_of_token() { e } }

    // Identifiers
    rule identifier() -> OwningCStr =
        n:quiet! { $(['_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) } {?
            if !env.reserved.contains(n) {
                Ok(OwningCStr::copy_str(n))
            } else {
                Err("identifier")
            }
        } /
        expected!("identifier")

    // Constants
    rule dec() = ['0'..='9']
    rule oct() = ['0'..='7']
    rule hex() = ['0'..='9' | 'a'..='f' | 'A'..='F']

    pub rule constant() -> Box<AstExpr> =
        numeric_constant() / character_constant()

    rule numeric_constant() -> Box<AstExpr> =
        n:quiet! { integer_number() } { ast_expr_constant_create(n) } /
        expected!("number")

    rule integer_number() -> libc::c_int =
        n:$(['1'..='9'] dec()* / "0") end_of_token() {
            parse_int(n)
        }

    rule character_constant() -> Box<AstExpr> =
        c:quiet! { $("'" character()+ "'") } {
            ast_expr_constant_create_char(parse_char(c))
        } /
        expected!("character")

    rule character() = [^ '\'' | '\\' | '\n'] / "\\" [^ '\n']

    // String literals
    rule string_literal() -> Box<AstExpr> =
        "\"" a:$(string_char()*) "\"" {
            ast_expr_literal_create(OwningCStr::copy_str(a))
        }

    rule string_char() = [^ '"' | '\\' | '\n'] / "\\" [^ '\n']

    // 6.5.1 Primary expression
    rule primary_expression() -> Box<AstExpr> =
        a:identifier() {
            ast_expr_identifier_create(a)
        } /
        "$" {
            ast_expr_arbarg_create()
        } /
        constant() /
        string_literal() /
        "(" _ a:expression() _ ")" { a }

    // 6.5.2 Postfix operators
    rule postfix_expression() -> Box<AstExpr> =
        a:primary_expression() ops:postfix_op()* {
            let mut a = a;
            for op in ops {
                a = match op {
                    PostfixOp::ArrayAccess(i) =>
                        ast_expr_unary_create(
                            ast_expr_binary_create(a, AstBinaryOp::Addition, i),
                            AstUnaryOp::Dereference,
                        ),
                    PostfixOp::Call(args) =>
                        ast_expr_call_create(a, args),
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
        } /
        allocation_expression()

    rule postfix_op() -> PostfixOp =
        _ "[" _ i:expression() _ "]" { PostfixOp::ArrayAccess(i) } /
        _ "(" _ ")" { PostfixOp::Call(vec![]) } /
        _ "(" _ args:argument_expression_list() _ ")" { PostfixOp::Call(args) } /
        _ "." _ m:identifier() { PostfixOp::Dot(m) } /
        _ "->" _ m:identifier() { PostfixOp::Arrow(m) } /
        _ "++" { PostfixOp::Inc } /
        _ "--" { PostfixOp::Dec }

    rule allocation_expression() -> Box<AstExpr> =
        ".malloc" _ "(" a:expression() ")" { ast_expr_alloc_create(a) } /
        ".free" _ "(" a:expression() ")" { ast_expr_dealloc_create(a) } /
        ".clump" _ "(" a:expression() ")" { ast_expr_clump_create(a) }

    rule argument_expression_list() -> Vec<Box<AstExpr>> =
        args:cs1(<assignment_expression()>) { args }

    rule isdeallocand_expression() -> Box<AstExpr> =
        postfix_expression() /
        "@" _ a:isdeallocand_expression() { ast_expr_isdeallocand_create(a) } /
        "$" _ a:isdeallocand_expression() { ast_expr_isdereferencable_create(a) }

    // 6.5.3 Unary operators
    rule unary_expression() -> Box<AstExpr> =
        isdeallocand_expression() /
        "++" _ a:unary_expression() { ast_expr_incdec_create(a, true, true) } /
        "--" _ a:unary_expression() { ast_expr_incdec_create(a, false, true) } /
        op:unary_operator() _ e:cast_expression() { ast_expr_unary_create(e, op) } /
        "sizeof" _ "(" _ ty:type_name() _ ")" { ast_expr_constant_create(1) /* XXX */ }

    rule unary_operator() -> AstUnaryOp =
        "&" !"&" { AstUnaryOp::Address } /
        "*" { AstUnaryOp::Dereference } /
        "+" { AstUnaryOp::Positive } /
        "-" { AstUnaryOp::Negative } /
        "~" { AstUnaryOp::OnesComplement } /
        "!" { AstUnaryOp::Bang }

    // 6.5.4 Cast expressions
    rule cast_expression() -> Box<AstExpr> = unary_expression()

    // 6.5.5 -- 6.5.14 Binary operators
    rule binary_expression() -> Box<AstExpr> = precedence! {
        x:(@) _ "||" _ y:@ { x }
        --
        x:(@) _ "&&" _ y:@ { x }
        --
        x:(@) _ "==" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Eq, y) }
        x:(@) _ "!=" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Ne, y) }
        --
        x:(@) _ "<" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Lt, y) }
        x:(@) _ ">" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Gt, y) }
        x:(@) _ "<=" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Le, y) }
        x:(@) _ ">=" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Ge, y) }
        --
        x:(@) _ "+" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Addition, y) }
        x:(@) _ "-" _ y:@ { ast_expr_binary_create(x, AstBinaryOp::Subtraction, y) }
        --
        x:(@) _ "*" _ y:@ { x }
        x:(@) _ "/" _ y:@ { x }
        x:(@) _ "%" _ y:@ { x }
        --
        e:cast_expression() { e }
    }

    rule conditional_expression() -> Box<AstExpr> = binary_expression()

    // 6.5.16 Assignment operators
    rule assignment_expression() -> Box<AstExpr> =
        a:unary_expression() _ assignment_operator() _ b:assignment_expression() {
            ast_expr_assignment_create(a, b)
        } /
        conditional_expression()

    rule assignment_operator() =
        "=" / ">>=" / "<<=" / "+=" / "-=" / "*=" / "/=" / "%=" / "&=" / "^=" / "|="

    pub rule expression() -> Box<AstExpr> = assignment_expression();

    // 6.6 Constant expressions
    rule constant_expression() -> Box<AstExpr> = conditional_expression()

    // 6.7 Declarations
    rule declaration() -> Declaration =
        t:declaration_specifiers() _ ";" {
            let name = ast_type_struct_tag(&t).cloned();
            Declaration { name, t }
        } /
        t:declaration_specifiers() _ v:declarator() _ ";" {
            let is_typedef = t.modifiers & MOD_TYPEDEF != 0;

            let mut t = t;
            for _ in 0..v.ptr_valence {
                t = ast_type_create_ptr(t);
            }
            if is_typedef {
                // Note: I think the original doesn't check for null here, so you can probably
                // crash it with `typedef int;` Certainly the handler for `declaration :
                // declaration_specifiers init_declarator_list ';'` in the original can create
                // a `struct declaration` with null `.name`, and I am sure it's not checked for
                // everywhere.
                if let Some(name) = &v.name {
                    env.add_typename(name.as_str());
                }
            }
            Declaration { name: v.name, t }
        }

    rule declaration_modifier() -> AstTypeModifier =
        storage_class_specifier() /
        type_qualifier()

    rule declaration_specifiers() -> Box<AstType> =
        type_specifier() /
        type_specifier() _ t:declaration_specifiers() { t } /
        m:declaration_modifier() _ s:declaration_specifiers() {
            let mut s = s;
            ast_type_mod_or(&mut s, m);
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

    rule type_specifier() -> Box<AstType> =
        K(<"void">) { ast_type_create(AstTypeBase::Void, 0) } /
        K(<"char">) { ast_type_create(AstTypeBase::Char, 0) } /
        K(<"int">) { ast_type_create(AstTypeBase::Int, 0) } /
        struct_or_union_specifier() /
        t:typedef_name() { ast_type_create_userdef(t) }

    rule typedef_name() -> OwningCStr = i:identifier() {?
        if env.is_typename(i.as_str()) {
            Ok(i)
        } else {
            Err("<unused>")
        }
    }

    // note: unions are unsupported in the original
    rule struct_or_union_specifier() -> Box<AstType> =
        "struct" _ tag:identifier() _ "{" _ fields:struct_declaration_list() _ "}" {
            ast_type_create_struct(Some(tag), Some(Box::new(fields)))
        } /
        "struct" _ "{" _ fields:struct_declaration_list() _ "}" {
            ast_type_create_struct_anonym(fields)
        } /
        "struct" _ tag:identifier() {
            ast_type_create_struct_partial(tag)
        }

    rule struct_declaration_list() -> Vec<Box<AstVariable>> =
        decls:list1(<struct_declaration()>) { decls }

    rule struct_declaration() -> Box<AstVariable> =
        d:declaration() {?
            if let Some(name) = d.name {
                Ok(ast_variable_create(name, d.t))
            } else {
                Err("variable with no name")
            }
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
            DirectFunctionDeclarator { name, params }
        } /
        name:identifier() _ "(" _ ")" {
            DirectFunctionDeclarator { name, params: vec![] }
        }

    rule declarator() -> Declarator =
        p:pointer() _ name:direct_declarator() { Declarator { ptr_valence: p, name: Some(name) } } /
        name:direct_declarator() { Declarator { ptr_valence: 0, name: Some(name) } } /
        p:pointer() { Declarator { ptr_valence: p, name: None } }

    rule direct_declarator() -> OwningCStr =
        // Note: declarator postfix syntax is ignored in the original
        name:identifier() direct_declarator_postfix()* { name }

    rule direct_declarator_postfix() =
        _ "[" _ constant_expression() _ "]" /
        _ "(" _ parameter_type_list() _ ")"

    rule pointer() -> libc::c_int =
        s:$("*"+) { s.len() as libc::c_int }

    rule parameter_type_list() -> Vec<Box<AstVariable>> = parameter_list()

    rule parameter_list() -> Vec<Box<AstVariable>> =
        decls:cs1(<parameter_declaration()>) { decls }

    rule parameter_declaration() -> Box<AstVariable> =
        t:declaration_specifiers() _ decl:declarator() {
            let mut t = t;
            for _ in 0..decl.ptr_valence {
                t = ast_type_create_ptr(t);
            }
            let name = decl.name.unwrap_or(OwningCStr::empty());
            ast_variable_create(name, t)
        } /
        t:declaration_specifiers() {
            ast_variable_create(OwningCStr::empty(), t)
        }

    rule type_name() -> Box<AstType> =
        t:specifier_qualifier_list() _ abstract_declarator() { t } /
        specifier_qualifier_list()

    rule abstract_declarator() -> libc::c_int = pointer()

    rule statement() -> Box<AstStmt> =
        labelled_statement() /
        block:compound_statement() p:position!() {
            ast_stmt_create_compound(env.lexloc(p), block)
        } /
        expression_statement() /
        selection_statement() /
        iteration_statement() /
        jump_statement() /
        iteration_effect_statement() /
        v:compound_verification_statement() p:position!() {
            ast_stmt_create_compound_v(env.lexloc(p), v)
        }

    rule specifier_qualifier_list() -> Box<AstType> =
        q:type_specifier() _ specifier_qualifier_list() { q } /
        q:type_specifier() { q } /
        // Note: original is I guess UB? type confusion
        type_qualifier() _ t:specifier_qualifier_list() { t } /
        type_qualifier() {
            ast_type_create(AstTypeBase::Int, 0)
        }

    rule labelled_statement() -> Box<AstStmt> =
        label:identifier() _ ":" _ s:statement() p:position!() {
            ast_stmt_create_labelled(env.lexloc(p), label, s)
        } /
        K(<"case">) _ constant_expression() _ ":" _ statement() p:position!() {
            ast_stmt_create_nop(env.lexloc(p))
        } /
        K(<"default">) _ ":" _ statement() p:position!() {
            ast_stmt_create_nop(env.lexloc(p))
        }

    rule compound_statement() -> Box<AstBlock> =
        "{" _ b:block() _ "}" { b }

    rule block() -> Box<AstBlock> =
        d:declaration_list() _ s:statement_list() {
            ast_block_create(d, s)
        } /
        d:declaration_list() {
            ast_block_create(d, vec![])
        } /
        s:statement_list() {
            ast_block_create(vec![], s)
        } /
        /* empty */ {
            ast_block_create(vec![], vec![])
        }

    rule iteration_effect_statement() -> Box<AstStmt> =
        "." _ s:for_iteration_statement(true) { s }

    rule compound_verification_statement() -> Box<AstBlock> =
        "~" _ "[" _ "]" {
            ast_block_create(vec![], vec![])
        } /
        "~" _ "[" _ b:block() _ "]" {
            b
        }

    rule declaration_list() -> Vec<Box<AstVariable>> =
        decls:list1(<declaration()>) { variable_array_from_decl_vec(decls) }

    rule statement_list() -> Vec<Box<AstStmt>> =
        stmts:list1(<statement()>) { stmts }

    rule expression_statement() -> Box<AstStmt> =
        ";" p:position!() { ast_stmt_create_nop(env.lexloc(p)) } /
        e:expression() _ ";" p:position!() { ast_stmt_create_expr(env.lexloc(p), e) }

    rule selection_statement() -> Box<AstStmt> =
        K(<"if">) _ "(" _ cond:expression() _ ")" _ then:statement() _ K(<"else">) _ alt:statement() p:position!() {
            let neg_cond = ast_expr_unary_create(ast_expr_copy(&cond), AstUnaryOp::Bang);
            let else_stmt = ast_stmt_create_sel(env.lexloc(p), false, neg_cond, alt, None);
            ast_stmt_create_sel(env.lexloc(p), false, cond, then, Some(else_stmt))
        } /
        K(<"if">) _ "(" _ cond:expression() _ ")" _ then:statement() p:position!() {
            ast_stmt_create_sel(env.lexloc(p), false, cond, then, None)
        } /
        K(<"switch">) _ "(" _ v:expression() _ ")" _ cases:statement() p:position!() {
            ast_stmt_create_nop(env.lexloc(p))
        }

    rule optional_compound_verification() -> Box<AstBlock> =
        vs:compound_verification_statement()? {
            vs.unwrap_or_else(|| {
                ast_block_create(vec![], vec![])
            })
        }

    rule for_iteration_statement(as_iteration_e: bool) -> Box<AstStmt> =
        K(<"for">) _ "(" _ init:expression_statement() _ cond:expression_statement() _ iter:expression() _ ")" _
        verif:optional_compound_verification() _ body:statement() p:position!() {
            ast_stmt_create_iter(env.lexloc(p), init, cond, iter, verif, body, as_iteration_e)
        }

    rule iteration_statement() -> Box<AstStmt> = for_iteration_statement(false)

    rule jump_statement() -> Box<AstStmt> =
        K(<"return">) _ expr:expression() _ ";" p:position!() {
            ast_stmt_create_jump(env.lexloc(p), AstJumpKind::Return, Some(expr))
        }

    rule block_statement() -> BlockStatement =
        ";" {
            BlockStatement { abstract_: None, body: None }
        } /
        c:compound_statement() {
            BlockStatement { abstract_: None, body: Some(c) }
        } /
        v:compound_verification_statement() _ ";" {
            BlockStatement { abstract_: Some(v), body: None }
        } /
        v:compound_verification_statement() _ c:compound_statement() {
            BlockStatement { abstract_: Some(v), body: Some(c) }
        }

    rule function_definition() -> BoxedFunction =
        K(<"axiom">) _ t:declaration_specifiers() _ decl:function_declarator() _ body:block_statement() {
            let mut t = t;
            for _ in 0..decl.ptr_valence {
                t = ast_type_create_ptr(t);
            }
            ast_function_create(
                true,
                t,
                decl.decl.name,
                decl.decl.params,
                body.abstract_.unwrap_or_else(||
                                              ast_block_create(vec![], vec![])),
                body.body.map(SemiBox::Owned),
            )
        } /
        t:declaration_specifiers() _ decl:function_declarator() _ body:block_statement() {
            let mut t = t;
            for _ in 0..decl.ptr_valence {
                t = ast_type_create_ptr(t);
            }
            ast_function_create(
                false,
                t,
                decl.decl.name,
                decl.decl.params,
                body.abstract_.unwrap_or_else(||
                                              ast_block_create(vec![], vec![])),
                body.body.map(SemiBox::Owned),
            )
        } /
        // Note: this was in the original. surely shouldn't be allowed, and implicit return type
        // should be int if it is allowed.
        decl:function_declarator() _ body:block_statement() {
            ast_function_create(
                false,
                ast_type_create(AstTypeBase::Void, 0),
                decl.decl.name,
                decl.decl.params,
                body.abstract_.unwrap_or_else(||
                                              ast_block_create(vec![], vec![])),
                body.body.map(SemiBox::Owned),
            )
        }

    rule external_declaration() -> Box<AstExternDecl> =
        f:function_definition() { ast_functiondecl_create(f) } /
        d:declaration() {?
            if let Some(name) = d.name {
                Ok(ast_decl_create(name, d.t))
            } else {
                Err("global declaration with no name")
            }
        }

    pub rule translation_unit() -> Box<Ast> =
        directive()? _ decl:list1(<external_declaration()>) _ { ast_from_vec(decl) }
}
}
