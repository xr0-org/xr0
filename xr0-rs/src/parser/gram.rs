use crate::ast::*;
use crate::parser::env::Env;
use crate::util::SemiBox;

pub struct Declaration {
    pub name: Option<String>,
    pub t: Box<AstType>,
}

pub struct DirectFunctionDeclarator {
    pub name: String,
    pub params: Vec<Box<AstVariable>>,
}

pub struct FunctionDeclarator {
    pub ptr_valence: usize,
    pub decl: DirectFunctionDeclarator,
}

pub struct Declarator {
    pub ptr_valence: usize,
    pub name: Option<String>,
}

struct BlockStatement {
    abstract_: Option<Box<AstBlock>>,
    body: Option<Box<AstBlock>>,
}

enum PostfixOp {
    ArrayAccess(Box<AstExpr>),
    Call(Vec<Box<AstExpr>>),
    Dot(String),
    Arrow(String),
    Inc,
    Dec,
}

fn variable_array_from_decl_vec(decls: Vec<Declaration>) -> Vec<Box<AstVariable>> {
    decls
        .into_iter()
        .map(|decl| AstVariable::new(decl.name.unwrap(), decl.t))
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
    rule identifier() -> String =
        n:quiet! { $(['_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) } {?
            if !env.reserved.contains(n) {
                Ok(n.to_string())
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
        n:quiet! { integer_number() } { AstExpr::new_constant(n) } /
        expected!("number")

    rule integer_number() -> c_int =
        n:$(['1'..='9'] dec()* / "0") end_of_token() {
            parse_int(n)
        }

    rule character_constant() -> Box<AstExpr> =
        c:quiet! { $("'" character()+ "'") } {
            AstExpr::new_constant_char(parse_char(c))
        } /
        expected!("character")

    rule character() = [^ '\'' | '\\' | '\n'] / "\\" [^ '\n']

    // String literals
    rule string_literal() -> Box<AstExpr> =
        "\"" a:$(string_char()*) "\"" {
            AstExpr::new_literal(a.to_string())
        }

    rule string_char() = [^ '"' | '\\' | '\n'] / "\\" [^ '\n']

    // 6.5.1 Primary expression
    rule primary_expression() -> Box<AstExpr> =
        a:identifier() {
            AstExpr::new_identifier(a)
        } /
        "$" {
            AstExpr::new_arbarg()
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
                        AstExpr::new_unary(
                            AstExpr::new_binary(a, AstBinaryOp::Addition, i),
                            AstUnaryOp::Dereference,
                        ),
                    PostfixOp::Call(args) =>
                        AstExpr::new_call(a, args),
                    PostfixOp::Dot(name) =>
                        AstExpr::new_member(a, name),
                    PostfixOp::Arrow(name) =>
                        AstExpr::new_member(
                            AstExpr::new_unary(
                                AstExpr::new_binary(
                                    a,
                                    AstBinaryOp::Addition,
                                    AstExpr::new_constant(0)
                                ),
                                AstUnaryOp::Dereference
                            ),
                            name,
                        ),
                    PostfixOp::Inc => AstExpr::new_incdec(a, true, false),
                    PostfixOp::Dec => AstExpr::new_incdec(a, false, false),
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
        ".malloc" _ "(" a:expression() ")" { AstExpr::new_alloc(a) } /
        ".free" _ "(" a:expression() ")" { AstExpr::new_dealloc(a) } /
        ".clump" _ "(" a:expression() ")" { AstExpr::new_clump(a) }

    rule argument_expression_list() -> Vec<Box<AstExpr>> =
        args:cs1(<assignment_expression()>) { args }

    rule isdeallocand_expression() -> Box<AstExpr> =
        postfix_expression() /
        "@" _ a:isdeallocand_expression() { AstExpr::new_isdeallocand(a) } /
        "$" _ a:isdeallocand_expression() { AstExpr::new_isdereferencable(a) }

    // 6.5.3 Unary operators
    rule unary_expression() -> Box<AstExpr> =
        isdeallocand_expression() /
        "++" _ a:unary_expression() { AstExpr::new_incdec(a, true, true) } /
        "--" _ a:unary_expression() { AstExpr::new_incdec(a, false, true) } /
        op:unary_operator() _ e:cast_expression() { AstExpr::new_unary(e, op) } /
        "sizeof" _ "(" _ ty:type_name() _ ")" { AstExpr::new_constant(1) /* XXX */ }

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
        x:(@) _ "==" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Eq, y) }
        x:(@) _ "!=" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Ne, y) }
        --
        x:(@) _ "<" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Lt, y) }
        x:(@) _ ">" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Gt, y) }
        x:(@) _ "<=" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Le, y) }
        x:(@) _ ">=" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Ge, y) }
        --
        x:(@) _ "+" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Addition, y) }
        x:(@) _ "-" _ y:@ { AstExpr::new_binary(x, AstBinaryOp::Subtraction, y) }
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
            AstExpr::new_assignment(a, b)
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
            let name = t.struct_tag().map(str::to_string);
            Declaration { name, t }
        } /
        t:declaration_specifiers() _ v:declarator() _ ";" {
            let is_typedef = t.modifiers & MOD_TYPEDEF != 0;

            let mut t = t;
            for _ in 0..v.ptr_valence {
                t = AstType::new_ptr(t);
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

    rule declaration_modifier() -> AstTypeModifiers =
        storage_class_specifier() /
        type_qualifier()

    rule declaration_specifiers() -> Box<AstType> =
        type_specifier() /
        type_specifier() _ t:declaration_specifiers() { t } /
        m:declaration_modifier() _ s:declaration_specifiers() {
            let mut s = s;
            s.mod_or(m);
            s
        }

    rule storage_class_specifier() -> AstTypeModifiers =
        K(<"typedef">) { MOD_TYPEDEF } /
        K(<"extern">) { MOD_EXTERN } /
        K(<"static">) { MOD_STATIC } /
        K(<"auto">) { MOD_AUTO } /
        K(<"register">) { MOD_REGISTER }

    rule type_specifier() -> Box<AstType> =
        K(<"void">) { AstType::new(AstTypeBase::Void, 0) } /
        K(<"char">) { AstType::new(AstTypeBase::Char, 0) } /
        K(<"int">) { AstType::new(AstTypeBase::Int, 0) } /
        struct_or_union_specifier() /
        t:typedef_name() { AstType::new_userdef(t) }

    rule typedef_name() -> String = i:identifier() {?
        if env.is_typename(i.as_str()) {
            Ok(i)
        } else {
            Err("<unused>")
        }
    }

    // note: unions are unsupported in the original
    rule struct_or_union_specifier() -> Box<AstType> =
        "struct" _ tag:identifier() _ "{" _ fields:struct_declaration_list() _ "}" {
            AstType::new_struct(Some(tag), Some(Box::new(fields)))
        } /
        "struct" _ "{" _ fields:struct_declaration_list() _ "}" {
            AstType::new_struct_anonym(fields)
        } /
        "struct" _ tag:identifier() {
            AstType::new_struct_partial(tag)
        }

    rule struct_declaration_list() -> Vec<Box<AstVariable>> =
        decls:list1(<struct_declaration()>) { decls }

    rule struct_declaration() -> Box<AstVariable> =
        d:declaration() {?
            if let Some(name) = d.name {
                Ok(AstVariable::new(name, d.t))
            } else {
                Err("variable with no name")
            }
        }

    rule type_qualifier() -> AstTypeModifiers =
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

    rule direct_declarator() -> String =
        // Note: Declarator postfix syntax is ignored in the original
        name:identifier() direct_declarator_postfix()* { name }

    rule direct_declarator_postfix() =
        _ "[" _ constant_expression() _ "]" /
        _ "(" _ parameter_type_list() _ ")"

    rule pointer() -> usize =
        s:$("*"+) { s.len() }

    rule parameter_type_list() -> Vec<Box<AstVariable>> = parameter_list()

    rule parameter_list() -> Vec<Box<AstVariable>> =
        decls:cs1(<parameter_declaration()>) { decls }

    rule parameter_declaration() -> Box<AstVariable> =
        t:declaration_specifiers() _ decl:declarator() {
            let mut t = t;
            for _ in 0..decl.ptr_valence {
                t = AstType::new_ptr(t);
            }
            let name = decl.name.unwrap_or("".to_string());
            AstVariable::new(name, t)
        } /
        t:declaration_specifiers() {
            AstVariable::new("".to_string(), t)
        }

    rule type_name() -> Box<AstType> =
        t:specifier_qualifier_list() _ abstract_declarator() { t } /
        specifier_qualifier_list()

    rule abstract_declarator() -> usize = pointer()

    rule statement() -> Box<AstStmt> =
        labelled_statement() /
        block:compound_statement() p:position!() {
            AstStmt::new_compound(env.lexloc(p), block)
        } /
        expression_statement() /
        selection_statement() /
        iteration_statement() /
        jump_statement() /
        iteration_effect_statement() /
        v:compound_verification_statement() p:position!() {
            AstStmt::new_compound_v(env.lexloc(p), v)
        }

    rule specifier_qualifier_list() -> Box<AstType> =
        q:type_specifier() _ specifier_qualifier_list() { q } /
        q:type_specifier() { q } /
        // Note: Original is I guess UB? Seems like type confusion.
        type_qualifier() _ t:specifier_qualifier_list() { t } /
        type_qualifier() {
            AstType::new(AstTypeBase::Int, 0)
        }

    rule labelled_statement() -> Box<AstStmt> =
        label:identifier() _ ":" _ s:statement() p:position!() {
            AstStmt::new_labelled(env.lexloc(p), label, s)
        } /
        K(<"case">) _ constant_expression() _ ":" _ statement() p:position!() {
            AstStmt::new_nop(env.lexloc(p))
        } /
        K(<"default">) _ ":" _ statement() p:position!() {
            AstStmt::new_nop(env.lexloc(p))
        }

    rule compound_statement() -> Box<AstBlock> =
        "{" _ b:block() _ "}" { b }

    rule block() -> Box<AstBlock> =
        d:declaration_list() _ s:statement_list() {
            AstBlock::new(d, s)
        } /
        d:declaration_list() {
            AstBlock::new(d, vec![])
        } /
        s:statement_list() {
            AstBlock::new(vec![], s)
        } /
        /* empty */ {
            AstBlock::new(vec![], vec![])
        }

    rule iteration_effect_statement() -> Box<AstStmt> =
        "." _ s:for_iteration_statement(true) { s }

    rule compound_verification_statement() -> Box<AstBlock> =
        "~" _ "[" _ "]" {
            AstBlock::new(vec![], vec![])
        } /
        "~" _ "[" _ b:block() _ "]" {
            b
        }

    rule declaration_list() -> Vec<Box<AstVariable>> =
        decls:list1(<declaration()>) { variable_array_from_decl_vec(decls) }

    rule statement_list() -> Vec<Box<AstStmt>> =
        stmts:list1(<statement()>) { stmts }

    rule expression_statement() -> Box<AstStmt> =
        ";" p:position!() { AstStmt::new_nop(env.lexloc(p)) } /
        e:expression() _ ";" p:position!() { AstStmt::new_expr(env.lexloc(p), e) }

    rule selection_statement() -> Box<AstStmt> =
        K(<"if">) _ "(" _ cond:expression() _ ")" _ then:statement() _ K(<"else">) _ alt:statement() p:position!() {
            AstStmt::new_sel(env.lexloc(p), false, cond, then, Some(alt))
        } /
        K(<"if">) _ "(" _ cond:expression() _ ")" _ then:statement() p:position!() {
            AstStmt::new_sel(env.lexloc(p), false, cond, then, None)
        } /
        K(<"switch">) _ "(" _ v:expression() _ ")" _ cases:statement() p:position!() {
            panic!()
        }

    rule optional_compound_verification() -> Box<AstBlock> =
        vs:compound_verification_statement()? {
            vs.unwrap_or_else(|| {
                AstBlock::new(vec![], vec![])
            })
        }

    rule for_iteration_statement(as_iteration_e: bool) -> Box<AstStmt> =
        K(<"for">) _ "(" _ init:expression_statement() _ cond:expression_statement() _ iter:expression() _ ")" _
        verif:optional_compound_verification() _ body:statement() p:position!() {
            AstStmt::new_iter(env.lexloc(p), init, cond, iter, verif, body, as_iteration_e)
        }

    rule iteration_statement() -> Box<AstStmt> = for_iteration_statement(false)

    rule jump_statement() -> Box<AstStmt> =
        K(<"return">) _ expr:expression() _ ";" p:position!() {
            AstStmt::new_jump(env.lexloc(p), AstJumpKind::Return, Some(expr))
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

    rule function_definition() -> Box<AstFunction<'static>> =
        K(<"axiom">) _ t:declaration_specifiers() _ decl:function_declarator() _ body:block_statement() {
            let mut t = t;
            for _ in 0..decl.ptr_valence {
                t = AstType::new_ptr(t);
            }
            ast_function_create(
                true,
                t,
                decl.decl.name,
                decl.decl.params,
                body.abstract_.unwrap_or_else(|| AstBlock::new(vec![], vec![])),
                body.body.map(SemiBox::Owned),
            )
        } /
        t:declaration_specifiers() _ decl:function_declarator() _ body:block_statement() {
            let mut t = t;
            for _ in 0..decl.ptr_valence {
                t = AstType::new_ptr(t);
            }
            ast_function_create(
                false,
                t,
                decl.decl.name,
                decl.decl.params,
                body.abstract_.unwrap_or_else(|| AstBlock::new(vec![], vec![])),
                body.body.map(SemiBox::Owned),
            )
        } /
        // Note: This was in the original. Surely shouldn't be allowed, and implicit return type
        // should be int if it is allowed.
        decl:function_declarator() _ body:block_statement() {
            ast_function_create(
                false,
                AstType::new(AstTypeBase::Void, 0),
                decl.decl.name,
                decl.decl.params,
                body.abstract_.unwrap_or_else(|| AstBlock::new(vec![], vec![])),
                body.body.map(SemiBox::Owned),
            )
        }

    rule external_declaration() -> Box<AstExternDecl> =
        f:function_definition() { AstExternDecl::new_function(f) } /
        d:declaration() {?
            if let Some(name) = d.name {
                Ok(AstExternDecl::new(name, d.t))
            } else {
                Err("global declaration with no name")
            }
        }

    pub rule translation_unit() -> Box<Ast> =
        directive()? _ decl:list1(<external_declaration()>) _ { ast_from_vec(decl) }
}
}
