%{
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "ast.h"
#include "gram_util.h"
#include "lex.h"
#include "util.h"

extern char *yytext;

int
yylex();

int
yyerror();

/* XXX */
int
int_constant(char *s)
{
	assert(strlen(s) == 1);
	return (int) *s - '0';
}

/* XXX */
int
char_constant(char *s)
{
	assert(strlen(s) == 3 && s[0] == '\'' && s[2] == '\'');
	return s[1];
}

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

int
effectfromid(char *id)
{
	if (strcmp(id, "alloc") == 0) {
		return EFFECT_ALLOC;
	} else if (strcmp(id, "dealloc") == 0) {
		return EFFECT_DEALLOC;
	} else if (strcmp(id, "undefined") == 0) {
		return EFFECT_UNDEFINED;
	}
	assert(false);
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

struct variable_array
variable_array_create(struct ast_variable *v)
{
	struct variable_array arr = (struct variable_array) {
		.n 	= 1, 
		.var	= malloc(sizeof(struct ast_variable *)),
	};
	arr.var[0] = v;
	return arr;
}

struct variable_array
variable_array_append(struct variable_array *arr, struct ast_variable *v)
{
	arr->var = realloc(arr->var, sizeof(struct ast_variable *) * ++arr->n);
	arr->var[arr->n-1] = v;
	return *arr;
}

struct expr_array
expr_array_create(struct ast_expr *v)
{
	struct expr_array arr = (struct expr_array) {
		.n 	= 1, 
		.expr	= malloc(sizeof(struct ast_expr *)),
	};
	arr.expr[0] = v;
	return arr;
}

struct expr_array
expr_array_append(struct expr_array *arr, struct ast_expr *v)
{
	arr->expr = realloc(arr->expr, sizeof(struct ast_expr *) * ++arr->n);
	arr->expr[arr->n-1] = v;
	return *arr;
}

%}

%token IDENTIFIER CONSTANT CHAR_LITERAL STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token EQV_OP IMPL_OP FLLW_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME
%token PREDICATE

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS
%token AXIOM LEMMA SFUNC ASSERT

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR SOME GOTO CONTINUE BREAK RETURN

%start translation_unit

%union {
	char *string;

	struct ast *ast;
	struct ast_block *block;
	struct ast_expr *expr;
	struct ast_externdecl *externdecl;
	struct ast_function *function;
	struct ast_stmt *statement;
	struct ast_type *type;
	struct ast_variable *variable;

	struct block_statement {
		struct ast_block *abstract, *body;
	} block_statement;

	struct expr_array expr_array;
	struct stmt_array stmt_array;
	struct variable_array variable_array;

	struct direct_function_declarator {
		char *name;
		int n;
		struct ast_variable **param;
	} direct_function_declarator;

	struct function_declarator {
		int ptr_valence;
		struct direct_function_declarator decl;
	} function_declarator;

	struct declarator {
		int ptr_valence;
		char *name;
	} declarator;

	struct type_specifier {
		int base;
		char *typedef_name;
	} type_specifier;

	int chain_operator;
	int integer;
	int type_modifier;
	int unary_operator;
}

%type <ast> translation_unit
%type <block> block compound_statement compound_verification_statement
%type <block> optional_compound_verification
%type <block_statement> block_statement
%type <chain_operator> connective equality_operator relational_operator
%type <declarator> declarator init_declarator init_declarator_list
%type <direct_function_declarator> direct_function_declarator

%type <expr> expression assignment_expression conditional_expression unary_expression
%type <expr> justified_expression postfix_expression primary_expression
%type <expr> logical_or_expression logical_and_expression inclusive_or_expression
%type <expr> exclusive_or_expression and_expression equality_expression
%type <expr> relational_expression shift_expression additive_expression
%type <expr> multiplicative_expression cast_expression
%type <expr> memory_expression

%type <expr_array> argument_expression_list

%type <externdecl> external_declaration
%type <function> function_definition
%type <function_declarator> function_declarator
%type <integer> pointer
%type <statement> statement expression_statement selection_statement jump_statement 
%type <statement> iteration_statement for_iteration_statement iteration_effect_statement
%type <stmt_array> statement_list
%type <string> identifier struct_or_union_specifier direct_declarator
%type <type> declaration_specifiers
%type <type_specifier> type_specifier
%type <type_modifier> declaration_modifier storage_class_specifier type_qualifier
%type <unary_operator> unary_operator
%type <variable> declaration parameter_declaration
%type <variable_array> declaration_list parameter_list parameter_type_list
%%

primary_expression
	: identifier
		{ $$ = ast_expr_create_identifier($1); }
	| CONSTANT
		{ $$ = ast_expr_create_constant(int_constant(yytext)); } /* XXX */
	| CHAR_LITERAL
		{ $$ = ast_expr_create_constant(char_constant(yytext)); }
	| STRING_LITERAL
		{ $$ = ast_expr_create_literal(strip_quotes(yytext)); }
	| '(' expression ')'
		{ $$ = ast_expr_create_bracketed($2); }
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
		{ $$ = ast_expr_create_access($1, $3); }
	| postfix_expression '(' ')'
		{ $$ = ast_expr_create_call($1, 0, NULL); }
	| postfix_expression '(' argument_expression_list ')'
		{ $$ = ast_expr_create_call($1, $3.n, $3.expr); }
	/*| postfix_expression '.' IDENTIFIER*/
	/*| postfix_expression PTR_OP IDENTIFIER*/
	| postfix_expression INC_OP
		{ $$ = ast_expr_create_incdec($1, true, false); }
	| postfix_expression DEC_OP
		{ $$ = ast_expr_create_incdec($1, false, false); }
	;

argument_expression_list
	: assignment_expression
		{ $$ = expr_array_create($1); }
	| argument_expression_list ',' assignment_expression
		{ $$ = expr_array_append(&$1, $3); }
	;

memory_expression
	: postfix_expression 
	| '.' identifier {
		$$ = ast_expr_create_memory(effectfromid($2), NULL);
		free($2);
	}
	| '.' identifier memory_expression	{
		$$ = ast_expr_create_memory(effectfromid($2), $3);
		free($2);
	}
	| ASSERT memory_expression {
		$$ = ast_expr_create_assertion($2);
	}

	;

unary_expression
	: memory_expression
	| INC_OP unary_expression
		{ $$ = ast_expr_create_incdec($2, true, true); }
	| DEC_OP unary_expression
		{ $$ = ast_expr_create_incdec($2, false, true); }
	| unary_operator cast_expression
		{ $$ = ast_expr_create_unary($2, $1); }
	/*| SIZEOF unary_expression*/
	/*| SIZEOF '(' type_name ')'*/
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
	/*| multiplicative_expression '*' cast_expression*/
	/*| multiplicative_expression '/' cast_expression*/
/*| multiplicative_expression '%' cast_expression*/
;

additive_expression
	: multiplicative_expression
	/*| additive_expression '+' multiplicative_expression*/
	/*| additive_expression '-' multiplicative_expression*/
	;

shift_expression
	: additive_expression
	/*| shift_expression LEFT_OP additive_expression*/
	/*| shift_expression RIGHT_OP additive_expression*/
	;

relational_operator
	: '<'	{ $$ = CHAIN_OP_LT; }
        | '>'	{ $$ = CHAIN_OP_GT; } 
        | LE_OP	{ $$ = CHAIN_OP_LE; }
        | GE_OP	{ $$ = CHAIN_OP_GE; }
	;

justification
	: '{' shift_expression '}'
	| /* empty */
	;

relational_expression
	: shift_expression
	| relational_expression relational_operator justification shift_expression
		{ $$ = ast_expr_create_chain($1, $2, NULL, $4); }
	;

equality_operator
	: EQ_OP	{ $$ = CHAIN_OP_EQ; }
	| NE_OP	{ $$ = CHAIN_OP_NE; }
	;

equality_expression
	: relational_expression
	| equality_expression equality_operator justification relational_expression
		{ $$ = ast_expr_create_chain($1, $2, NULL, $4); }
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
	/*| logical_and_expression AND_OP inclusive_or_expression*/
	;

logical_or_expression
	: logical_and_expression
	/*| logical_or_expression OR_OP logical_and_expression*/
	;

connective
	: EQV_OP 	{ $$ = CHAIN_OP_EQV; }
	| IMPL_OP	{ $$ = CHAIN_OP_IMPL; }
	/*| FLLW_OP	{ $$ = CHAIN_OP_FLLW; }*/
	;

justified_expression
	: logical_or_expression
	| justified_expression connective justification logical_or_expression
		{ $$ = ast_expr_create_chain($1, $2, NULL, $4); }
	;

conditional_expression
	: justified_expression
	/*| justified_expression '?' expression ':' conditional_expression*/
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
		{ $$ = ast_expr_create_assignment($1, $3); }
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
	: conditional_expression
	;

declaration
	: declaration_specifiers ';'
		{ $$ = ast_variable_create(dynamic_str(""), $1); }
	| declaration_specifiers init_declarator_list ';' {
		for (int i = 0; i < $2.ptr_valence; i++) {
			assert($1);
			$1 = ast_type_create_ptr($1);
		}
		$$ = ast_variable_create($2.name, $1);
	}
	;

declaration_modifier
	: storage_class_specifier
	| type_qualifier
	;

declaration_specifiers
	: type_specifier {
		switch ($1.base) {
		case TYPE_TYPEDEF:
			/* XXX */
			assert($1.typedef_name 
				&& strcmp($1.typedef_name, "FILE") == 0);
			$$ = ast_type_create_typedef(
				ast_type_create(TYPE_STRUCT, 0),
				$1.typedef_name
			);
			break;
		case TYPE_POINTER:
		case TYPE_ARRAY:
			assert(false);
		default:
			assert(!$1.typedef_name);
			$$ = ast_type_create($1.base, 0);
			break;
		}
	}
	/*| type_specifier declaration_specifiers {*/
		/*[> allocate for concatenation with intervening space <]*/
		/*$1 = realloc($1, strlen($1) + 1 + strlen($2->name) + 1);*/
		/*sprintf($1, "%s %s", $1, $2->name);*/
		/*free($2->name); $2->name = $1;*/
		/*$$ = $2;*/
	/*}*/
	| declaration_modifier declaration_specifiers {
		$2->mod |= $1;
		$$ = $2;
	}
	;

init_declarator_list
	: init_declarator
	/*| init_declarator_list ',' init_declarator*/
	;

init_declarator
	: declarator
	/*| declarator '=' initializer*/
	;

storage_class_specifier
	: TYPEDEF	{ 
		/* XXX: typedef is not modelled as a modifier, so our only
		 * concern is to prepare the lexer to return the next string it
		 * sees as a TYPE_NAME, so that in the type_specifier beneath
		 * the fact that this is a typedef will be captured */
		$$ = 0; installclass = IC_TYPE;
	}
	| EXTERN	{ $$ = MOD_EXTERN; }
	| STATIC	{ $$ = MOD_STATIC; }
	| AUTO		{ $$ = MOD_AUTO; }
	| REGISTER	{ $$ = MOD_REGISTER; }
	;

type_specifier
	: VOID		{ $$ = (struct type_specifier) { TYPE_VOID, NULL }; }
	| CHAR		{ $$ = (struct type_specifier) { TYPE_CHAR, NULL }; }
	/*| SHORT*/
	| INT		{ $$ = (struct type_specifier) { TYPE_INT, NULL }; }
	/*| LONG*/
	/*| FLOAT*/
	/*| DOUBLE*/
	| SIGNED	{ $$ = (struct type_specifier) { TYPE_SIGNED, NULL }; }
	| UNSIGNED	{ $$ = (struct type_specifier) { TYPE_UNSIGNED, NULL }; }
	| struct_or_union_specifier
		/* XXX: placeholder */
		{ free($1); $$ = (struct type_specifier) { TYPE_STRUCT, NULL }; }
	| enum_specifier
		{ assert(false); }
	| TYPE_NAME	{ 
		$$ = (struct type_specifier) { TYPE_TYPEDEF, dynamic_str(yytext) }; 
	}
	;

struct_or_union_specifier
	/*: struct_or_union IDENTIFIER '{' struct_declaration_list '}'*/
	/*| struct_or_union '{' struct_declaration_list '}'*/
	: struct_or_union identifier { $$ = $2; }
	;

struct_or_union
	: STRUCT
	| UNION
	;

/*struct_declaration_list*/
	/*: struct_declaration*/
	/*| struct_declaration_list struct_declaration*/
	/*;*/

/*struct_declaration*/
	/*: specifier_qualifier_list struct_declarator_list ';'*/
	/*;*/

/*specifier_qualifier_list*/
	/*: type_specifier specifier_qualifier_list*/
	/*| type_specifier*/
	/*| type_qualifier specifier_qualifier_list*/
	/*| type_qualifier*/
	/*;*/

/*struct_declarator_list*/
	/*: struct_declarator*/
	/*| struct_declarator_list ',' struct_declarator*/
	/*;*/

/*struct_declarator*/
	/*: declarator*/
	/*| ':' constant_expression*/
	/*| declarator ':' constant_expression*/
	/*;*/

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: IDENTIFIER
	| IDENTIFIER '=' constant_expression
	;

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
			.n		= $3.n,
			.param		= $3.var,
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
	: pointer direct_declarator	{ $$ = (struct declarator) { $1, $2 }; }
	| direct_declarator		{ $$ = (struct declarator) { 0, $1 }; }
	;

direct_declarator
	: identifier	/* XXX */
	/*| '(' declarator ')'*/
	/*| direct_declarator '[' constant_expression ']'*/
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
		$$ = variable_array_append(&$1, $3);
	}
	;

parameter_declaration
	: declaration_specifiers declarator {
		for (int i = 0; i < $2.ptr_valence; i++) {
			assert($1);
			$1 = ast_type_create_ptr($1);
		}
		$$ = ast_variable_create($2.name, $1);
	}
	/*| declaration_specifiers abstract_declarator*/
	| declaration_specifiers {
		$$ = ast_variable_create(dynamic_str(""), $1);
	}
	;

/*identifier_list*/
	/*: IDENTIFIER*/
	/*| identifier_list ',' IDENTIFIER*/
	/*;*/

/*type_name*/
	/*: specifier_qualifier_list*/
	/*| specifier_qualifier_list abstract_declarator*/
	/*;*/

/*abstract_declarator*/
	/*: pointer*/
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

/*initializer*/
	/*: assignment_expression*/
	/*| '{' initializer_list '}'*/
	/*| '{' initializer_list ',' '}'*/
	/*;*/

/*initializer_list*/
	/*: initializer*/
	/*| initializer_list ',' initializer*/
	/*;*/

statement
	/*: labeled_statement*/
	: compound_statement
		{ $$ = ast_stmt_create_compound(lexloc(), $1); }
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	| iteration_effect_statement
		{ $$ = ast_stmt_create_iter_e($1); }
	| compound_verification_statement
		{ $$ = ast_stmt_create_compound_v(lexloc(), $1); }
	;

/*labeled_statement*/
	/*: IDENTIFIER ':' statement*/
	/*| CASE constant_expression ':' statement*/
	/*| DEFAULT ':' statement*/
	/*;*/

compound_statement
	: '{' block '}' { $$ = $2; }
	;

block
	: /* empty */
		{ $$ = ast_block_create(NULL, 0, NULL, 0); }
	| statement_list
		{ $$ = ast_block_create(NULL, 0, $1.stmt, $1.n); }
	| declaration_list
		{ $$ = ast_block_create($1.var, $1.n, NULL, 0); }
	| declaration_list statement_list
		{ $$ = ast_block_create($1.var, $1.n, $2.stmt, $2.n); }
	;

iteration_effect_statement
	: '.' for_iteration_statement
		{ $$ = $2; }

compound_verification_statement
	: '[' ']'	{ $$ = ast_block_create(NULL, 0, NULL, 0); }
	| '[' block ']'	{ $$ = $2; }
	;

declaration_list
	: declaration
		{ $$ = variable_array_create($1); }
	| declaration_list declaration
		{ $$ = variable_array_append(&$1, $2); }
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
	: IF '(' expression ')' statement
		{ $$ = ast_stmt_create_sel(lexloc(), false, $3, $5, NULL); }
	| IF '(' expression ')' statement ELSE statement
		{
			struct ast_expr *neg_cond = ast_expr_create_unary(
				ast_expr_copy($3), UNARY_OP_BANG
			);
			struct ast_stmt *_else = ast_stmt_create_sel(
				lexloc(), false, neg_cond, $7, NULL
			); 
			$$ = ast_stmt_create_sel(lexloc(), false, $3, $5, _else);
		}
	// | SWITCH '(' expression ')' statement
	;

for_some
	: FOR
	| SOME
	;

optional_compound_verification
	: /* empty */ { $$ = ast_block_create(NULL, 0, NULL, 0); }
	| compound_verification_statement
	;

for_iteration_statement
	/* : for_some '(' expression_statement expression_statement ')' statement
		{ $$ = ast_stmt_create_iter(lexloc(), NULL, $3, $4, $6); }
	*/
	: for_some '(' expression_statement expression_statement expression ')' optional_compound_verification statement
		{
		struct ast_stmt *stmt = $8; 
		$$ = ast_stmt_create_iter(lexloc(), $3, $4, $5, $7, $8); }
	;

iteration_statement
	/*: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	*/
	: for_iteration_statement
	;

jump_statement
	/*: GOTO IDENTIFIER ';'*/
	/*| CONTINUE ';'*/
	/*| BREAK ';'*/
	/*| RETURN ';'*/
	: RETURN expression ';'
		{ $$ = ast_stmt_create_jump(lexloc(), JUMP_RETURN, $2); }
	;

translation_unit
	: external_declaration
		{ root = ast_create($1); }
	| translation_unit external_declaration
		{ root = ast_append(root, $2); }
	;

external_declaration
	: function_definition	{ $$ = ast_functiondecl_create($1); }
	| declaration		{ $$ = ast_variabledecl_create($1); }
	;

block_statement
	: ';' {
		$$ = (struct block_statement) {
			.abstract	= NULL,
			.body		= NULL,
		};
	}
	| compound_statement {
		$$ = (struct block_statement) {
			.abstract	= NULL,
			.body		= $1,
		};
	}
	| compound_verification_statement ';' {
		$$ = (struct block_statement) {
			.abstract	= $1,
			.body		= NULL,
		};
	}
	| compound_verification_statement compound_statement {
		$$ = (struct block_statement) {
			.abstract	= $1,
			.body		= $2,
		};
	}
	;

function_definition
	: AXIOM declaration_specifiers function_declarator block_statement {
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
			$4.abstract ? $4.abstract : ast_block_create(NULL, 0, NULL, 0),
			$4.body
		);
	}
	| declaration_specifiers function_declarator block_statement {
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
			$3.abstract ? $3.abstract : ast_block_create(NULL, 0, NULL, 0),
			$3.body
		);
	}
	| function_declarator block_statement { 
		$$ = ast_function_create(
			false,
			ast_type_create(TYPE_VOID, 0),
			$1.decl.name,
			$1.decl.n,
			$1.decl.param,
			$2.abstract ? $2.abstract : ast_block_create(NULL, 0, NULL, 0),
			$2.body
		);
	}
	;

%%
#include <stdio.h>

extern struct lexememarker marker; 

int
yyerror(char *s)
{
	fflush(stdout);
	if (!marker.filename) {
		fprintf(stderr, "error: %s with no lexeme marker\n", s);
		exit(EXIT_FAILURE);
	}
	if (verbose) {
		fprintf(stderr, "\n%*s\n", marker.column, "^");
	}
	fprintf(stderr, "%s:%d:%d: %s\n",
		marker.filename, marker.linenum, marker.column, s);
	exit(EXIT_FAILURE);
}
