%{
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "ast.h"
#include "gram_util.h"
#include "intern.h"
#include "lex.h"
#include "literals.h"
#include "util.h"

#include "expr.h"
#include "stmt.h"
#include "type.h"

struct namedseq *FUNC_SEQ = NULL;

char *CURR_FUNC = NULL;
int CURR_RCONST_COUNT = 0;

void
setcurrfunc(char *func)
{
	if (FUNC_SEQ) {
		namedseq_destroy(FUNC_SEQ);
	}
	FUNC_SEQ = namedseq_create(dynamic_str(func));
}

char *
getrconstkey(void)
{
	assert(FUNC_SEQ);
	return namedseq_next(FUNC_SEQ);
}

extern char *yytext;

int
yylex(void);

int
yyerror(char *);

/* XXX */
char *
strip_quotes(char *s)
{
	assert(s[0] == '"'); /* TODO support wide characters */
	int len = strlen(s) - 2 + 1;
	char *t = malloc(sizeof(char) * len);
	snprintf(t, len, "%s", s + 1);
	return t;
}

struct stmt_array
stmt_array_create(struct ast_stmt *v)
{
	struct stmt_array arr = (struct stmt_array) {
		.n	= 1, 
		.stmt	= malloc(sizeof(struct ast_stmt *)),
	};
	arr.stmt[0] = v;
	return arr;
}

struct stmt_array
stmt_array_append(struct stmt_array *arr, struct ast_stmt *v)
{
	arr->stmt = realloc(arr->stmt, sizeof(struct ast_stmt *) * ++arr->n);
	arr->stmt[arr->n-1] = v;
	return *arr;
}

struct stmt_array
stmt_array_concat(struct stmt_array *arr1, struct stmt_array *arr2)
{
	for (int i = 0; i < arr2->n; i++) {
		stmt_array_append(arr1, ast_stmt_copy(arr2->stmt[i]));
	}
	return *arr1;
}

struct expr_array
expr_array_create(struct ast_expr *e)
{
	struct expr_array arr = (struct expr_array) {
		.n 	= 1, 
		.expr	= malloc(sizeof(struct ast_expr *)),
	};
	arr.expr[0] = e;
	return arr;
}

struct expr_array
expr_array_append(struct expr_array *arr, struct ast_expr *e) {
	arr->expr = realloc(arr->expr, sizeof(struct ast_expr *) * ++arr->n);
	arr->expr[arr->n-1] = e;
	return *arr;
}

struct ast_variable_arr *
variable_array_append(struct ast_variable_arr *arr, struct ast_variable *v)
{
	ast_variable_arr_append(arr, v);
	return arr;
}

struct ast_variable_arr *
variable_array_create(struct ast_variable *v)
{
	return variable_array_append(ast_variable_arr_create(), v);
}


%}

%token IDENTIFIER CONSTANT CHAR_LITERAL STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token ISDEALLOCAND_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS
%token AXIOM

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token TK_ALLOC TK_DEALLOC TK_CLUMP

%token START_AST START_EXPR

%union {
	char *string;

	struct ast *ast;
	struct ast_block *block;
	struct ast_expr *expr;
	struct ast_externdecl *externdecl;
	struct ast_function *function;
	struct lexememarker *lexememarker;
	struct ast_stmt *stmt;
	struct ast_type *type;
	struct ast_variable *variable;

	struct block_statement {
		struct ast_block *abstract, *body;
	} block_statement;

	struct expr_array expr_array;
	struct stmt_array stmt_array;
	struct ast_variable_arr *variable_array;

	struct direct_function_declarator {
		char *name;
		int n;
		struct ast_variable **param;
	} direct_function_declarator;

	struct function_declarator {
		int ptr_valence;
		struct direct_function_declarator decl;
	} function_declarator;

	int binary_operator;
	int integer;
	int type_modifier;
	int unary_operator;
	int boolean;
}

%type <ast> translation_unit
%type <block> block compound_statement compound_verification_statement
%type <block> optional_compound_verification
%type <block_statement> block_statement
%type <binary_operator> equality_operator relational_operator
%type <boolean> struct_or_union
%type <direct_function_declarator> direct_function_declarator

%type <expr> expression assignment_expression conditional_expression unary_expression
%type <expr> postfix_expression primary_expression
%type <expr> logical_or_expression logical_and_expression inclusive_or_expression
%type <expr> exclusive_or_expression and_expression equality_expression
%type <expr> relational_expression shift_expression additive_expression
%type <expr> multiplicative_expression cast_expression
%type <expr> range_expression range_operand
%type <expr> allocation_expression isdeallocand_expression constant_expression
%type <expr> declarator init_declarator initializer
%type <expr> direct_declarator

%type <expr_array> argument_expression_list init_declarator_list

%type <externdecl> external_declaration
%type <function> function_definition
%type <function_declarator> function_declarator
%type <integer> pointer abstract_declarator 
%type <lexememarker> selection_statement_if
%type <stmt> statement expression_statement selection_statement jump_statement 
%type <stmt> labelled_statement iteration_statement
%type <stmt_array> statement_list declaration_list
%type <string> identifier
%type <type> declaration_specifiers type_specifier struct_or_union_specifier 
%type <type_modifier> declaration_modifier storage_class_specifier type_qualifier
%type <unary_operator> unary_operator
%type <variable> parameter_declaration struct_declaration
%type <variable_array> parameter_list parameter_type_list
%type <variable_array> struct_declaration_list
%type <variable_array> declaration

%start start

%%

start
	: START_AST translation_unit
	| START_EXPR expression
		{ YACC_PARSED_EXPR = $2; }
	;

primary_expression
	: identifier
		{ $$ = ast_expr_identifier_create($1); }
	| CONSTANT
		{ $$ = ast_expr_constant_create(parse_int(yytext)); } /* XXX */
	| CHAR_LITERAL
		{ $$ = ast_expr_constant_create_char(parse_char(yytext)); }
	| STRING_LITERAL
		{ $$ = ast_expr_literal_create(strip_quotes(yytext)); }
	| '(' expression ')'
		{ $$ = ast_expr_bracketed_create($2); }
	;

allocation_expression
	: TK_ALLOC '(' expression ')'
		{ $$ = ast_expr_alloc_create($3); }
	| TK_DEALLOC '(' expression ')'
		{ $$ = ast_expr_dealloc_create($3); }
	| TK_CLUMP '(' expression ')'
		{ $$ = ast_expr_clump_create($3); }
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']' {
		$$ = ast_expr_unary_create(
			ast_expr_binary_create($1, BINARY_OP_ADDITION, $3),
			UNARY_OP_DEREFERENCE
		);
	}
	| postfix_expression '(' ')'
		{ $$ = ast_expr_call_create($1, 0, NULL); }
	| postfix_expression '(' argument_expression_list ')'
		{ $$ = ast_expr_call_create($1, $3.n, $3.expr); }
	| postfix_expression '.' identifier
		{ $$ = ast_expr_member_create($1, $3); }
	| postfix_expression PTR_OP identifier {
		$$ = ast_expr_member_create(
			ast_expr_unary_create($1, UNARY_OP_DEREFERENCE),
			$3
		);
	}
	| postfix_expression INC_OP
		{ $$ = ast_expr_incdec_create($1, true, false); }
	| postfix_expression DEC_OP
		{ $$ = ast_expr_incdec_create($1, false, false); }
	| allocation_expression
	;

argument_expression_list
	: assignment_expression
		{ $$ = expr_array_create($1); }
	| argument_expression_list ',' assignment_expression
		{ $$ = expr_array_append(&$1, $3); }
	;

isdeallocand_expression
	: postfix_expression 
	| ISDEALLOCAND_OP isdeallocand_expression {
		$$ = ast_expr_isdeallocand_create($2);
	}
	;

unary_expression
	: isdeallocand_expression
	| INC_OP unary_expression
		{ $$ = ast_expr_incdec_create($2, true, true); }
	| DEC_OP unary_expression
		{ $$ = ast_expr_incdec_create($2, false, true); }
	| unary_operator cast_expression
		{ $$ = ast_expr_unary_create($2, $1); }
	/*| SIZEOF unary_expression*/
	| SIZEOF '(' type_name ')'
		{ $$ = ast_expr_constant_create(1); /* XXX */ }
	;

unary_operator
	: '&'	{ $$ = UNARY_OP_ADDRESS; }
	| '*'	{ $$ = UNARY_OP_DEREFERENCE; }
	| '+'	{ $$ = UNARY_OP_POSITIVE; }
	| '-'	{ $$ = UNARY_OP_NEGATIVE; }
	| '~'	{ $$ = UNARY_OP_ONES_COMPLEMENT; }
	| '!'	{ $$ = UNARY_OP_BANG; }
	;

cast_expression
	: unary_expression
	/*| '(' type_name ')' cast_expression*/
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression
	| multiplicative_expression '/' cast_expression
	| multiplicative_expression '%' cast_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression
		{ $$ = ast_expr_binary_create($1, BINARY_OP_ADDITION, $3); }
	| additive_expression '-' multiplicative_expression
		{ $$ = ast_expr_binary_create($1, BINARY_OP_SUBTRACTION, $3); }
	;

range_expression
	: additive_expression
	| '[' range_operand '?' range_operand ']' {
		$$ = ast_expr_range_create(
			getrconstkey(),
			$2 ? $2 : ast_expr_rangemin_create(),
			$4 ? $4 : ast_expr_rangemax_create()
		);
	}
	| '[' range_operand '!' range_operand ']' {
		$$ = ast_expr_range_createnokey(
			$2 ? $2 : ast_expr_rangemin_create(),
			$4 ? $4 : ast_expr_rangemax_create()
		);
	}
	;

range_operand
	: additive_expression
	| /* empty */		{ $$ = NULL; }
	;

shift_expression
	: range_expression
	| shift_expression LEFT_OP range_expression
	| shift_expression RIGHT_OP range_expression
	;

relational_operator
	: '<'	{ $$ = BINARY_OP_LT; }
        | '>'	{ $$ = BINARY_OP_GT; } 
        | LE_OP	{ $$ = BINARY_OP_LE; }
        | GE_OP	{ $$ = BINARY_OP_GE; }
	;

relational_expression
	: shift_expression
	| relational_expression relational_operator shift_expression
		{ $$ = ast_expr_binary_create($1, $2, $3); }
	;

equality_operator
	: EQ_OP	{ $$ = BINARY_OP_EQ; }
	| NE_OP	{ $$ = BINARY_OP_NE; }
	;

equality_expression
	: relational_expression
	| equality_expression equality_operator relational_expression
		{ $$ = ast_expr_binary_create($1, $2, $3); }
	;

and_expression
	: equality_expression
	/*| and_expression '&' equality_expression*/
	;

exclusive_or_expression
	: and_expression
	/*| exclusive_or_expression '^' and_expression*/
	;

inclusive_or_expression
	: exclusive_or_expression
	/*| inclusive_or_expression '|' exclusive_or_expression*/
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
		{ $$ = ast_expr_assignment_create($1, $3); }
	;

assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

expression
	: assignment_expression
	/*| expression ',' assignment_expression*/
	;

constant_expression
	/* XXX: not evaluating compile-time constant expressions for now */ 
	: conditional_expression
	;

declaration
	: declaration_specifiers ';' {
		/* XXX */
		char *name = ast_type_struct_tag($1);
		assert(name);
		struct ast_variable_arr *vars = ast_variable_arr_create();
		ast_variable_arr_append(
			vars, ast_variable_create(dynamic_str(name), ast_type_copy($1))
		);
		$$ = vars;
	}
	| declaration_specifiers init_declarator_list ';' {
		struct expr_array expr_arr = $2; 
		struct ast_variable_arr *vars = ast_variable_arr_create();
		for (int i = 0; i < expr_arr.n; i++) {
			struct ast_declaration *decl = ast_expr_declare(
				ast_expr_declarator(expr_arr.expr[i]), $1
			);
			struct ast_variable *v = ast_variable_create(
				dynamic_str(ast_declaration_name(decl)),
				ast_type_copy(ast_declaration_type(decl))
			);
			ast_variable_setinit(v, ast_expr_initialiser(expr_arr.expr[i]));
			ast_variable_arr_append(vars, v);
		}
		assert(ast_variable_arr_n(vars) > 0);
		$$ = vars;
	}
	;

declaration_modifier
	: storage_class_specifier
	| type_qualifier
	;

declaration_specifiers
	: type_specifier {
		$$ = $1;
	}
	| type_specifier declaration_specifiers {
		$$ = $2;
	}
	| declaration_modifier declaration_specifiers {
		ast_type_mod_or($2, $1);
		$$ = $2;
	}
	;

init_declarator_list
	: init_declarator {
		$$ = expr_array_create($1);
	}
	| init_declarator_list ',' init_declarator {
		$$ = expr_array_append(&$1, $3);
	}
	;

init_declarator
	: declarator
	| declarator '=' initializer {
		$$ = ast_expr_assignment_create($1, $3);	
	}
	;

storage_class_specifier
	: TYPEDEF	{ 
		/* XXX: typedef is not modelled as a modifier, so our only
		 * concern is to prepare the lexer to return the next string it
		 * sees as a TYPE_NAME, so that in the type_specifier beneath
		 * the fact that this is a typedef will be captured */
		$$ = MOD_TYPEDEF; installclass = IC_TYPE;
	}
	| EXTERN	{ $$ = MOD_EXTERN; }
	| STATIC	{ $$ = MOD_STATIC; }
	| AUTO		{ $$ = MOD_AUTO; }
	| REGISTER	{ $$ = MOD_REGISTER; }
	;

type_specifier
	: VOID		{ $$ = ast_type_create(TYPE_VOID, 0); }
	| CHAR		{ $$ = ast_type_create(TYPE_CHAR, 0); }
	/*| SHORT*/
	| INT		{ $$ = ast_type_create(TYPE_INT, 0); }
	/*| LONG*/
	/*| FLOAT*/
	/*| DOUBLE*/
	/*| SIGNED*/
	/*| UNSIGNED*/
	| struct_or_union_specifier
	/*| enum_specifier*/
	| TYPE_NAME	{ $$ = ast_type_create_userdef(dynamic_str(yytext)); }
	;

struct_or_union_specifier
	: struct_or_union identifier '{' struct_declaration_list '}'
		{ assert($1); $$ = ast_type_create_struct($2, $4); }
	| struct_or_union '{' struct_declaration_list '}'
		{ assert($1); $$ = ast_type_create_struct_anonym($3); }
	| struct_or_union identifier
		{ assert($1); $$ = ast_type_create_struct_partial($2); }
	;

struct_or_union
	: STRUCT	{ $$ = true; }
	| UNION		{ $$ = false; }
	;

struct_declaration_list
	: struct_declaration
		{ $$ = variable_array_create($1); }
	| struct_declaration_list struct_declaration
		{ $$ = variable_array_append($1, $2); }
	;

struct_declaration
	/*: specifier_qualifier_list struct_declarator_list ';'*/
	: declaration /* XXX: added temporarily */ {
		assert(ast_variable_arr_n($1) == 1);
		$$ = ast_variable_arr_v($1)[0];
	}
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

/*struct_declarator_list*/
	/*: struct_declarator*/
	/*| struct_declarator_list ',' struct_declarator*/
	/*;*/

/*struct_declarator*/
	/*: declarator*/
	/*| ':' constant_expression*/
	/*| declarator ':' constant_expression*/
	/*;*/

/*enum_specifier*/
	/*: ENUM '{' enumerator_list '}'*/
	/*| ENUM IDENTIFIER '{' enumerator_list '}'*/
	/*| ENUM IDENTIFIER*/
	/*;*/

/*enumerator_list*/
	/*: enumerator*/
	/*| enumerator_list ',' enumerator*/
	/*;*/

/*enumerator*/
	/*: IDENTIFIER*/
	/*| IDENTIFIER '=' constant_expression*/
	/*;*/

type_qualifier
	: CONST		{ $$ = MOD_CONST; }
	| VOLATILE	{ $$ = MOD_VOLATILE; }
	;

function_declarator
	: pointer direct_function_declarator	{
		$$ = (struct function_declarator) {
			.ptr_valence = $1, .decl = $2,
		};
	}
	| direct_function_declarator		{
		$$ = (struct function_declarator) {
			.ptr_valence = 0, .decl = $1,
		};
	}
	;

identifier
	: IDENTIFIER { $$ = dynamic_str(yytext); }
	;

direct_function_declarator
	: identifier '(' parameter_type_list ')' {
		$$ = (struct direct_function_declarator) {
			.name		= $1,
			.n		= ast_variable_arr_n($3),
			.param		= ast_variable_arr_v($3),
		};
	}
	| identifier '(' ')' {
		$$ = (struct direct_function_declarator) {
			.name		= $1,
			.n		= 0,
			.param		= NULL,
		};
	}
	;

declarator
	: pointer direct_declarator {
		$$ = $2;
		for (int i = 0; i < $1; i++) {
			$$ = ast_expr_unary_create($$, UNARY_OP_DEREFERENCE);
		}
	}
	| direct_declarator
	;

direct_declarator
	: identifier 
		{ $$ = ast_expr_identifier_create($1); }
	| '(' declarator ')'
		{ $$ = $2; }
	| direct_declarator '[' constant_expression ']' {
		$$ = ast_expr_unary_create(
			ast_expr_binary_create($1, BINARY_OP_ADDITION, $3),
			UNARY_OP_DEREFERENCE
		);
	}
	/*| direct_declarator '[' ']'*/
	/*| direct_declarator '(' parameter_type_list ')'*/
	/*| direct_declarator '(' identifier_list ')'*/
	/*| direct_declarator '(' ')'*/
	;

pointer
	: '*'		{ $$ = 1; }
	/*| '*' type_qualifier_list*/
	| '*' pointer	{ $$ = $2 + 1; }
	/*| '*' type_qualifier_list pointer*/
	;

/*type_qualifier_list*/
	/*: type_qualifier*/
	/*| type_qualifier_list type_qualifier*/
	/*;*/

parameter_type_list
	: parameter_list 
	/*| parameter_list ',' ELLIPSIS*/
	;

parameter_list
	: parameter_declaration {
		$$ = variable_array_create($1);
	}
	| parameter_list ',' parameter_declaration {
		$$ = variable_array_append($1, $3);
	}
	;

parameter_declaration
	: declaration_specifiers declarator {
		struct ast_declaration *decl = ast_expr_declare($2, $1);
		$$ = ast_variable_create(
			ast_declaration_name(decl),
			ast_declaration_type(decl)
		);
	}
	| declaration_specifiers abstract_declarator {
		for (int i = 0; i < $2; i++) {
			$1 = ast_type_create_ptr($1);
		}
		$$ = ast_variable_create(NULL, $1);
	}
	| declaration_specifiers {
		$$ = ast_variable_create(dynamic_str(""), $1);
	}
	;

/*identifier_list*/
	/*: IDENTIFIER*/
	/*| identifier_list ',' IDENTIFIER*/
	/*;*/

type_name
	: specifier_qualifier_list
	| specifier_qualifier_list abstract_declarator
	;

abstract_declarator
	: pointer 
	;
	/*| direct_abstract_declarator*/
	/*| pointer direct_abstract_declarator*/
	/*;*/

/*direct_abstract_declarator*/
	/*: '(' abstract_declarator ')'*/
	/*| '[' ']'*/
	/*| '[' constant_expression ']'*/
	/*| direct_abstract_declarator '[' ']'*/
	/*| direct_abstract_declarator '[' constant_expression ']'*/
	/*| '(' ')'*/
	/*| '(' parameter_type_list ')'*/
	/*| direct_abstract_declarator '(' ')'*/
	/*| direct_abstract_declarator '(' parameter_type_list ')'*/
	/*;*/

initializer
	: assignment_expression
	/*| '{' initializer_list '}'*/
	/*| '{' initializer_list ',' '}'*/
	;

/*initializer_list*/
	/*: initializer*/
	/*| initializer_list ',' initializer*/
	/*;*/

statement
	: labelled_statement
	| compound_statement
		{ $$ = ast_stmt_create_compound(lexloc(), $1); }
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	| compound_verification_statement
		{ $$ = ast_stmt_create_compound_v(lexloc(), $1); }
	;

labelled_statement
	: identifier ':' statement
		{ $$ = ast_stmt_create_labelled(lexloc(), $1, $3); }
	| CASE constant_expression ':' statement
		{ $$ = ast_stmt_create_nop(lexloc()); }
	| DEFAULT ':' statement
		{ $$ = ast_stmt_create_nop(lexloc()); }
	;

compound_statement
	: '{' block '}' { $$ = $2; }
	;

block
	: /* empty */ {
		$$ = ast_block_create(NULL, 0);
	}
	| statement_list {
		$$ = ast_block_create($1.stmt, $1.n);
	}
	| declaration_list {
		$$ = ast_block_create($1.stmt, $1.n);
	}
	| declaration_list statement_list {
		struct stmt_array new = stmt_array_concat(&$1, &$2);
		$$ = ast_block_create(new.stmt, new.n);
	}
	;

compound_verification_statement
	: '~' '[' ']'		{ $$ = ast_block_create(NULL, 0); }
	| '~' '[' block ']'	{ $$ = $3; }
	;

declaration_list
	: declaration {
		$$ = stmt_array_create(ast_stmt_create_declaration(lexloc(), $1));
	}
	| declaration_list declaration {
		$$ = stmt_array_append(&$1, ast_stmt_create_declaration(lexloc(), $2));
	}
	;

statement_list
	: statement
		{ $$ = stmt_array_create($1); }
	| statement_list statement
		{ $$ = stmt_array_append(&$1, $2); }
	;

expression_statement
	: ';' 			{ $$ = ast_stmt_create_nop(lexloc()); }
	| expression ';'	{ $$ = ast_stmt_create_expr(lexloc(), $1); }
	;

selection_statement
	: selection_statement_if '(' expression ')' statement
		{ $$ = ast_stmt_create_sel($1, false, $3, ast_stmt_as_compound($5), NULL); }
	| selection_statement_if '(' expression ')' statement ELSE statement
		{ $$ = ast_stmt_create_sel($1, false, $3, ast_stmt_as_compound($5), ast_stmt_as_compound($7));
		}
	| SWITCH '(' expression ')' statement
		{ assert(false); }
	;

selection_statement_if
	: IF { $$ = lexloc(); }
	;

optional_compound_verification
	: /* empty */ { $$ = ast_block_create(NULL, 0); }
	| compound_verification_statement
	;

iteration_statement
	/*: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	*/
	: FOR '(' expression_statement expression_statement expression ')' optional_compound_verification statement
		{ $$ = ast_stmt_create_for(lexloc(), $3, $4, $5, $7, $8); }
	;

jump_statement
	/*: GOTO IDENTIFIER ';'*/
	/*| CONTINUE ';'*/
	: BREAK ';'
		{ $$ = ast_stmt_create_break(lexloc()); }
	/*| RETURN ';'*/
	| RETURN expression ';'
		{ $$ = ast_stmt_create_return(lexloc(), $2); }
	;

translation_unit
	: external_declaration
		{ root = ast_create($1); }
	| translation_unit external_declaration
		{ root = ast_append(root, $2); }
	;

external_declaration
	: function_definition	{ $$ = ast_functiondecl_create($1); }
	| declaration		{
		assert(ast_variable_arr_n($1) == 1);
		struct ast_variable *v = ast_variable_arr_v($1)[0];
		$$ = ast_decl_create(ast_variable_name(v), ast_variable_type(v));
	}
	;

block_statement
	: ';' {
		$$ = (struct block_statement) {
			.abstract	= NULL,
			.body		= NULL,
		};
	}
	/* {} */
	| compound_statement {
		$$ = (struct block_statement) {
			.abstract	= NULL,
			.body		= $1,
		};
	}
	/* ~ []; */
	| compound_verification_statement ';' {
		$$ = (struct block_statement) {
			.abstract	= $1,
			.body		= NULL,
		};
	}
	/* ~ [] { ... } */
	| compound_verification_statement compound_statement {
		assert($2);
		$$ = (struct block_statement) {
			.abstract	= $1,
			.body		= $2,
		};
	}
	;

function_definition
	: AXIOM declaration_specifiers function_declarator 
		{ setcurrfunc($3.decl.name); }
		block_statement {
		for (int i = 0; i < $3.ptr_valence; i++) {
			assert($2);
			$2 = ast_type_create_ptr($2);
		}
		$$ = ast_function_create(
			true,
			$2,
			$3.decl.name,
			$3.decl.n,
			$3.decl.param,
			$5.abstract,
			$5.body,
			lexloc()
		);
	}
	| declaration_specifiers function_declarator
		{ setcurrfunc($2.decl.name); }
		block_statement {
		for (int i = 0; i < $2.ptr_valence; i++) {
			assert($1);
			$1 = ast_type_create_ptr($1);
		}
		$$ = ast_function_create(
			false,
			$1,
			$2.decl.name,
			$2.decl.n,
			$2.decl.param,
			$4.abstract,
			$4.body,
			lexloc()
		);
	}
	| function_declarator
		{ setcurrfunc($1.decl.name); }
		block_statement { 
		$$ = ast_function_create(
			false,
			ast_type_create(TYPE_VOID, 0),
			$1.decl.name,
			$1.decl.n,
			$1.decl.param,
			$3.abstract,
			$3.body,
			lexloc()
		);
	}
	;

%%
#include <stdio.h>

extern struct lexememarker marker; 
extern enum loglevel LOG_LEVEL;

int
yyerror(char *s)
{
	fflush(stdout);
	if (!marker.filename) {
		fprintf(stderr, "error: %s with no lexeme marker\n", s);
		exit(EXIT_FAILURE);
	}
	if (LOG_LEVEL == LOG_INFO) {
		fprintf(stderr, "\n%*s\n", marker.column, "^");
	}
	char *mark = lexememarker_str(&marker);
	fprintf(stderr, "%s: %s\n", mark, s);
	free(mark);
	exit(EXIT_FAILURE);
}
