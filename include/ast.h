#ifndef XR0_AST_H
#define XR0_AST_H
#include <stdbool.h>

struct value;
struct error;

struct result;

struct result *
result_error_create(struct error *err);

struct result *
result_value_create(struct value *val);

void
result_destroy(struct result *);

bool
result_iserror(struct result *);

struct error *
result_as_error(struct result *);

struct value *
result_as_value(struct result *);

bool
result_hasvalue(struct result *);

struct object;
struct ast_type;

struct lvalue *
lvalue_create(struct ast_type *, struct object *);

void
lvalue_destroy(struct lvalue *);

struct ast_type *
lvalue_type(struct lvalue *);

struct object *
lvalue_object(struct lvalue *);


enum ast_expr_kind {
	EXPR_IDENTIFIER		= 1 << 0,
	EXPR_CONSTANT		= 1 << 1,
	EXPR_STRING_LITERAL	= 1 << 2,
	EXPR_BRACKETED		= 1 << 3,
	EXPR_ITERATION		= 1 << 4,

	EXPR_CALL		= 1 << 5,
	EXPR_INCDEC		= 1 << 6,

	EXPR_STRUCTMEMBER	= 1 << 7,

	EXPR_UNARY		= 1 << 8,
	EXPR_BINARY		= 1 << 9,

	EXPR_ASSIGNMENT		= 1 << 10,

	EXPR_ISDEALLOCAND	= 1 << 11,
	EXPR_ARBARG		= 1 << 12,
};

enum ast_unary_operator {
	UNARY_OP_ADDRESS		= 1 << 0,
	UNARY_OP_DEREFERENCE		= 1 << 1,
	UNARY_OP_POSITIVE		= 1 << 2,
	UNARY_OP_NEGATIVE		= 1 << 3,
	UNARY_OP_ONES_COMPLEMENT	= 1 << 4,
	UNARY_OP_BANG			= 1 << 5
};

enum ast_binary_operator {
	BINARY_OP_EQV	= 1 << 0,
	BINARY_OP_IMPL	= 1 << 1,
	BINARY_OP_FLLW	= 1 << 2,

	BINARY_OP_EQ	= 1 << 3,
	BINARY_OP_NE	= 1 << 4,

	BINARY_OP_LT	= 1 << 5,
	BINARY_OP_GT	= 1 << 6,
	BINARY_OP_LE	= 1 << 7,
	BINARY_OP_GE	= 1 << 8,				

	BINARY_OP_ADDITION	= 1 << 9,
	BINARY_OP_SUBTRACTION	= 1 << 10
};

struct ast_expr;

struct ast_expr *
ast_expr_identifier_create(char *);

char *
ast_expr_as_identifier(struct ast_expr *);

struct ast_expr *
ast_expr_constant_create(int);

int
ast_expr_as_constant(struct ast_expr *expr);

char *
ast_expr_as_literal(struct ast_expr *);

struct ast_expr *
ast_expr_literal_create(char *);

struct ast_expr *
ast_expr_bracketed_create(struct ast_expr *);

struct ast_expr *
ast_expr_iteration_create();

struct ast_expr *
ast_expr_call_create(struct ast_expr *, int narg, struct ast_expr **arg);

struct ast_expr *
ast_expr_call_root(struct ast_expr *);

int
ast_expr_call_nargs(struct ast_expr *);

struct ast_expr **
ast_expr_call_args(struct ast_expr *);

struct ast_expr *
ast_expr_incdec_create(struct ast_expr *, bool inc, bool pre);

struct ast_expr *
ast_expr_incdec_to_assignment(struct ast_expr *expr);

struct ast_expr *
ast_expr_incdec_root(struct ast_expr *);

bool
ast_expr_incdec_pre(struct ast_expr *);

struct ast_expr *
ast_expr_member_create(struct ast_expr *, char *);

struct ast_expr *
ast_expr_member_root(struct ast_expr *);

char *
ast_expr_member_field(struct ast_expr *);

struct ast_expr *
ast_expr_unary_create(struct ast_expr *, enum ast_unary_operator);

enum ast_unary_operator
ast_expr_unary_op(struct ast_expr *);

struct ast_expr *
ast_expr_unary_operand(struct ast_expr *);

struct ast_expr *
ast_expr_binary_create(struct ast_expr *e1, enum ast_binary_operator,
		struct ast_expr *e2);

struct ast_expr *
ast_expr_binary_e1(struct ast_expr *);

struct ast_expr *
ast_expr_binary_e2(struct ast_expr *);

enum ast_binary_operator
ast_expr_binary_op(struct ast_expr *);

struct ast_expr *
ast_expr_assignment_create(struct ast_expr *root, struct ast_expr *value);

struct ast_expr *
ast_expr_assignment_lval(struct ast_expr *expr);

struct ast_expr *
ast_expr_assignment_rval(struct ast_expr *expr);

struct ast_expr *
ast_expr_isdeallocand_create(struct ast_expr *assertand);

struct ast_expr *
ast_expr_isdeallocand_assertand(struct ast_expr *expr);

struct ast_expr *
ast_expr_arbarg_create();

void
ast_expr_destroy(struct ast_expr *);

char *
ast_expr_str(struct ast_expr *);

struct ast_expr *
ast_expr_copy(struct ast_expr *);

enum ast_expr_kind
ast_expr_kind(struct ast_expr *);

bool
ast_expr_equal(struct ast_expr *e1, struct ast_expr *e2);

struct math_state;

bool
ast_expr_matheval(struct ast_expr *e);

struct state;

bool
ast_expr_decide(struct ast_expr *, struct state *);

bool
ast_expr_rangedecide(struct ast_expr *, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

struct error *
ast_expr_exec(struct ast_expr *, struct state *);

struct lvalue *
ast_expr_lvalue(struct ast_expr *, struct state *);

struct result *
ast_expr_eval(struct ast_expr *, struct state *);

struct result *
ast_expr_absexec(struct ast_expr *, struct state *);

struct ast_stmt;

struct ast_block;
struct ast_variable;

struct ast_block *
ast_block_create(
	struct ast_variable **decl, int ndecl, 
	struct ast_stmt **stmt, int nstmt
);

void
ast_block_destroy(struct ast_block *);

char *
ast_block_str(struct ast_block *);

struct ast_block *
ast_block_copy(struct ast_block *b);

int
ast_block_ndecls(struct ast_block *b);

struct ast_variable **
ast_block_decls(struct ast_block *b);

int
ast_block_nstmts(struct ast_block *b);

struct ast_stmt **
ast_block_stmts(struct ast_block *b);

enum ast_stmt_kind {
	STMT_NOP		= 1 << 0,
	STMT_LABELLED		= 1 << 1,
	STMT_COMPOUND		= 1 << 2,
	STMT_COMPOUND_V		= 1 << 3,
	STMT_EXPR		= 1 << 4,
	STMT_SELECTION		= 1 << 5,
	STMT_ITERATION		= 1 << 6,
	STMT_ITERATION_E	= 1 << 7,
	STMT_JUMP		= 1 << 8,
	STMT_ALLOCATION		= 1 << 9,
};

enum ast_jump_kind {
	JUMP_RETURN	= 1 << 0,
};

struct ast_stmt;

struct lexememarker;

struct ast_stmt *
ast_stmt_create_labelled(struct lexememarker *, char *label, struct ast_stmt *);

char *
ast_stmt_labelled_label(struct ast_stmt *);

struct ast_stmt *
ast_stmt_labelled_stmt(struct ast_stmt *);

struct ast_stmt *
ast_stmt_create_nop(struct lexememarker *);

struct ast_stmt *
ast_stmt_create_expr(struct lexememarker *, struct ast_expr *);

struct ast_stmt *
ast_stmt_create_sel(struct lexememarker *, bool isswitch, struct ast_expr *cond,
		struct ast_stmt *body, struct ast_stmt *nest);

struct ast_expr *
ast_stmt_sel_cond(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_sel_body(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_sel_nest(struct ast_stmt *stmt);


struct ast_stmt *
ast_stmt_create_iter(struct lexememarker *,
		struct ast_stmt *init, struct ast_stmt *cond,
		struct ast_expr *iter, struct ast_block *abstract,
		struct ast_stmt *body);

struct ast_stmt *
ast_stmt_iter_init(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_iter_cond(struct ast_stmt *stmt);

struct ast_expr *
ast_stmt_iter_iter(struct ast_stmt *stmt);

struct ast_block *
ast_stmt_iter_abstract(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_iter_body(struct ast_stmt *stmt);

struct ast_expr *
ast_stmt_iter_lower_bound(struct ast_stmt *stmt);

struct ast_expr *
ast_stmt_iter_upper_bound(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_create_iter_e(struct ast_stmt *iter_stmt);

struct ast_stmt *
ast_stmt_create_compound(struct lexememarker *, struct ast_block *);

struct ast_block *
ast_stmt_as_block(struct ast_stmt *);

struct ast_stmt *
ast_stmt_create_compound_v(struct lexememarker *, struct ast_block *);

struct ast_stmt *
ast_stmt_create_jump(struct lexememarker *, enum ast_jump_kind, struct ast_expr *rv);

struct ast_expr *
ast_stmt_jump_rv(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_create_alloc(struct lexememarker *, struct ast_expr *arg);

struct ast_stmt *
ast_stmt_create_dealloc(struct lexememarker *, struct ast_expr *arg);

struct ast_expr *
ast_stmt_alloc_arg(struct ast_stmt *);

bool
ast_stmt_alloc_isalloc(struct ast_stmt *);

void
ast_stmt_destroy(struct ast_stmt *);

struct ast_stmt *
ast_stmt_copy(struct ast_stmt *);

char *
ast_stmt_str(struct ast_stmt *);

bool
ast_stmt_equal(struct ast_stmt *, struct ast_stmt *);

enum ast_stmt_kind
ast_stmt_kind(struct ast_stmt *);

/* ast_stmt_as_block: Wrap in block if single statement. */
struct ast_block *
ast_stmt_as_block(struct ast_stmt *);

struct ast_block *
ast_stmt_as_v_block(struct ast_stmt *);

struct ast_expr *
ast_stmt_as_expr(struct ast_stmt *);

struct result *
ast_stmt_absexec(struct ast_stmt *stmt, struct state *state);


enum ast_type_modifier {
	/* storage class */
	MOD_EXTERN	= 1 << 0,
	MOD_STATIC	= 1 << 1,
	MOD_AUTO	= 1 << 2,
	MOD_REGISTER	= 1 << 3,

	/* qualifier */
	MOD_CONST	= 1 << 4,
	MOD_VOLATILE	= 1 << 5,
};

enum ast_type_base { /* base type */
	TYPE_VOID,
	TYPE_CHAR,
	TYPE_SHORT,
	TYPE_INT,
	TYPE_LONG,
	TYPE_FLOAT,
	TYPE_DOUBLE,
	TYPE_SIGNED,
	TYPE_UNSIGNED,

	TYPE_POINTER,
	TYPE_ARRAY,

	TYPE_TYPEDEF,

	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_ENUM,
};

struct ast_type;

struct ast_type *
ast_type_create(enum ast_type_base base, enum ast_type_modifier mod);

/* TODO: allow modifiers for pointer, array and typedef types */

struct ast_type *
ast_type_create_ptr(struct ast_type *type);

struct ast_type *
ast_type_create_arr(struct ast_type *type, int length);

struct ast_type *
ast_type_create_typedef(struct ast_type *type, char *name);

struct ast_variable_arr;

struct ast_type *
ast_type_create_struct(char *tag, struct ast_variable_arr *);

struct ast_variable_arr *
ast_type_struct_members(struct ast_type *t);

char *
ast_type_struct_tag(struct ast_type *t);

struct ast_type *
ast_type_create_struct_anonym(struct ast_variable_arr *);

struct ast_type *
ast_type_create_struct_partial(char *tag);

void
ast_type_mod_or(struct ast_type *, enum ast_type_modifier);

void
ast_type_destroy(struct ast_type *);

char *
ast_type_str(struct ast_type *);

struct ast_type *
ast_type_copy(struct ast_type *t);

enum ast_type_base
ast_type_base(struct ast_type *t);

struct ast_type *
ast_type_ptr_type(struct ast_type *t);

struct ast_variable_arr;

struct ast_variable_arr *
ast_variable_arr_create();

void
ast_variable_arr_append(struct ast_variable_arr *, struct ast_variable *);

void
ast_variable_arr_destroy(struct ast_variable_arr *);

int
ast_variable_arr_n(struct ast_variable_arr *);

struct ast_variable **
ast_variable_arr_v(struct ast_variable_arr *);

struct ast_variable_arr *
ast_variable_arr_copy(struct ast_variable_arr *arr);

struct ast_variable;

struct ast_variable *
ast_variable_create(char *name, struct ast_type *type);

void
ast_variable_destroy(struct ast_variable *);

struct ast_variable *
ast_variable_copy(struct ast_variable *);

struct ast_variable **
ast_variables_copy(int n, struct ast_variable**);

char *
ast_variable_str(struct ast_variable *);

char *
ast_variable_name(struct ast_variable *);

struct ast_type *
ast_variable_type(struct ast_variable *);

struct ast_function;

/* ast_function_create: name must be allocated on the heap */
struct ast_function *
ast_function_create(
	bool isaxiom,
	struct ast_type *ret,
	char *name,
	int nparam,
	struct ast_variable **param,
	struct ast_block *abstract, 
	struct ast_block *body
);

void
ast_function_destroy(struct ast_function *);

char *
ast_function_str(struct ast_function *f);

char *
ast_function_name(struct ast_function *f);

struct ast_function *
ast_function_copy(struct ast_function *);

bool
ast_function_isaxiom(struct ast_function *f);

struct ast_type *
ast_function_type(struct ast_function *f);

struct ast_block *
ast_function_body(struct ast_function *f);

struct ast_block *
ast_function_abstract(struct ast_function *f);

int
ast_function_nparams(struct ast_function *f);

struct ast_variable **
ast_function_params(struct ast_function *f);

struct externals;
struct error;

struct error *
ast_function_verify(struct ast_function *, struct externals *);

struct result *
ast_function_absexec(struct ast_function *, struct state *state);

enum ast_externdecl_kind {
	EXTERN_FUNCTION = 1 << 0,
	EXTERN_VARIABLE	= 1 << 1,
	EXTERN_TYPE	= 1 << 2,
};

struct ast_externdecl;

struct ast_externdecl *
ast_functiondecl_create(struct ast_function *);

struct ast_function *
ast_externdecl_as_function(struct ast_externdecl *);

struct ast_externdecl *
ast_variabledecl_create(struct ast_variable *);

struct ast_externdecl *
ast_typedecl_create(struct ast_type *);

enum ast_externdecl_kind
ast_externdecl_kind(struct ast_externdecl *);

void
ast_externdecl_install(struct ast_externdecl *decl, struct externals *ext);

void
ast_externdecl_destroy(struct ast_externdecl *);

struct ast {
	int n;
	struct ast_externdecl **decl;
};

struct ast *
ast_create(struct ast_externdecl *);

struct ast *
ast_append(struct ast *, struct ast_externdecl *);

void
ast_destroy(struct ast *);

#endif
