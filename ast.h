#ifndef XR0_AST_H
#define XR0_AST_H
#include <stdbool.h>

struct ast_expr {
	enum ast_expr_kind {
		EXPR_IDENTIFIER		= 1 << 0,
		EXPR_CONSTANT		= 1 << 1,
		EXPR_STRING_LITERAL	= 1 << 2,
		EXPR_BRACKETED		= 1 << 3,
		EXPR_ITERATION		= 1 << 4,

		EXPR_ACCESS		= 1 << 5,
		EXPR_CALL		= 1 << 6,
		EXPR_INCDEC		= 1 << 7,

		EXPR_UNARY		= 1 << 8,
		EXPR_BINARY		= 1 << 9,

		EXPR_CHAIN		= 1 << 10,

		EXPR_ASSIGNMENT		= 1 << 11,

		EXPR_MEMORY		= 1 << 12,
		EXPR_ASSERTION		= 1 << 13,
	} kind;
	struct ast_expr *root;
	union {
		char *string; /* identifier, literal, assertion */
		int constant;
		struct {
			struct ast_expr *index;
		} access;
		struct {
			int n;
			struct ast_expr **arg;
		} call;
		struct {
			bool inc, pre;
		} incdec;
		enum ast_unary_operator {
			UNARY_OP_ADDRESS		= 1 << 0,
			UNARY_OP_DEREFERENCE		= 1 << 1,
			UNARY_OP_POSITIVE		= 1 << 2,
			UNARY_OP_NEGATIVE		= 1 << 3,
			UNARY_OP_ONES_COMPLEMENT	= 1 << 4,
			UNARY_OP_BANG			= 1 << 5,
		} unary_op;
		struct {
			enum ast_binary_operator {
				BINARY_OP_ADDITION	= 1 << 0,
			} op;
			struct ast_expr *e1, *e2;
		} binary;
		struct {
			enum ast_chain_operator {
				CHAIN_OP_EQV	= 1 << 0,
				CHAIN_OP_IMPL	= 1 << 1,
				CHAIN_OP_FLLW	= 1 << 2,

				CHAIN_OP_EQ	= 1 << 3,
				CHAIN_OP_NE	= 1 << 4,
				
				CHAIN_OP_LT	= 1 << 5,
				CHAIN_OP_GT	= 1 << 6,
				CHAIN_OP_LE	= 1 << 7,
				CHAIN_OP_GE	= 1 << 8,
			} op;
			struct ast_expr *justification, *last;
		} chain;
		struct ast_expr *assignment_value;
		struct {
			enum effect_kind {
				EFFECT_ALLOC		= 1 << 0,
				EFFECT_DEALLOC		= 1 << 1,
				EFFECT_UNDEFINED	= 1 << 2,
			} kind;
		} memory;
	} u;
};

struct ast_expr *
ast_expr_create_identifier(char *);

char *
ast_expr_as_identifier(struct ast_expr *);

struct ast_expr *
ast_expr_create_constant(int);

int
ast_expr_as_constant(struct ast_expr *expr);

struct ast_expr *
ast_expr_create_literal(char *);

struct ast_expr *
ast_expr_create_bracketed(struct ast_expr *);

struct ast_expr *
ast_expr_create_iteration();

struct ast_expr *
ast_expr_create_access(struct ast_expr *root, struct ast_expr *index);

struct ast_expr *
ast_expr_access_root(struct ast_expr *expr);

struct ast_expr *
ast_expr_access_index(struct ast_expr *expr);

struct ast_expr *
ast_expr_create_call(struct ast_expr *, int narg, struct ast_expr **arg);

struct ast_expr *
ast_expr_call_root(struct ast_expr *);

int
ast_expr_call_nargs(struct ast_expr *);

struct ast_expr **
ast_expr_call_args(struct ast_expr *);

struct ast_expr *
ast_expr_create_incdec(struct ast_expr *, bool inc, bool pre);

struct ast_expr *
ast_expr_create_unary(struct ast_expr *, enum ast_unary_operator);

enum ast_unary_operator
ast_expr_unary_op(struct ast_expr *);

struct ast_expr *
ast_expr_unary_operand(struct ast_expr *);

struct ast_expr *
ast_expr_create_binary(struct ast_expr *e1, enum ast_binary_operator,
		struct ast_expr *e2);

struct ast_expr *
ast_expr_binary_e1(struct ast_expr *);

struct ast_expr *
ast_expr_binary_e2(struct ast_expr *);

enum ast_binary_operator
ast_expr_binary_op(struct ast_expr *);

struct ast_expr *
ast_expr_create_chain(struct ast_expr *root, enum ast_chain_operator,
		struct ast_expr *justification, struct ast_expr *last);

struct ast_expr *
ast_expr_create_assignment(struct ast_expr *root, struct ast_expr *value);

struct ast_expr *
ast_expr_assignment_lval(struct ast_expr *expr);

struct ast_expr *
ast_expr_assignment_rval(struct ast_expr *expr);

struct ast_expr *
ast_expr_create_memory(enum effect_kind, struct ast_expr *expr);

struct ast_expr *
ast_expr_memory_root(struct ast_expr *expr);

bool
ast_expr_memory_isalloc(struct ast_expr *expr);

bool
ast_expr_memory_isunalloc(struct ast_expr *expr);

bool
ast_expr_memory_isundefined(struct ast_expr *expr);

enum effect_kind
ast_expr_memory_kind(struct ast_expr *expr);

struct ast_expr *
ast_expr_create_assertion(struct ast_expr *assertand);

struct ast_expr *
ast_expr_assertion_assertand(struct ast_expr *expr);

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

struct ast_stmt;

struct ast_block {
	int ndecl, nstmt;
	struct ast_variable **decl;
	struct ast_stmt **stmt;
};

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

struct ast_stmt {
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
	} kind;
	union {
		struct {
			char *label;
			struct ast_stmt *stmt;
		} labelled;
		struct ast_block *compound;
		struct {
			bool isswitch;
			struct ast_expr *cond;
			struct ast_stmt *body;
			struct ast_stmt *nest;
		} selection;
		struct {
			struct ast_stmt *init, *cond, *body;
			struct ast_expr *iter;
			struct ast_block *abstract;
		} iteration;
		struct ast_expr *expr;
		struct {
			enum ast_jump_kind {
				JUMP_RETURN	= 1 << 0,
			} kind;
			struct ast_expr *rv;
		} jump;
	} u;

	struct lexememarker *loc;
};

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

struct ast_type {
	enum ast_type_modifier {
		/* storage class */
		MOD_EXTERN	= 1 << 0,
		MOD_STATIC	= 1 << 1,
		MOD_AUTO	= 1 << 2,
		MOD_REGISTER	= 1 << 3,

		/* qualifier */
		MOD_CONST	= 1 << 4,
		MOD_VOLATILE	= 1 << 5,
	} mod;
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
	} base;
	union {
		struct ast_type *ptr_type;
		struct {
			struct ast_type *type;
			int length;
		} arr;
		struct {
			struct ast_type *type;
			char *name;
		} _typedef;
	} u;
};

struct ast_type *
ast_type_create(enum ast_type_base base, enum ast_type_modifier mod);

/* TODO: allow modifiers for pointer, array and typedef types */

struct ast_type *
ast_type_create_ptr(struct ast_type *type);

struct ast_type *
ast_type_create_arr(struct ast_type *type, int length);

struct ast_type *
ast_type_create_typedef(struct ast_type *type, char *name);

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

struct ast_variable {
	char *name;
	struct ast_type *type;
};

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

struct ast_function {
	bool isaxiom;

	struct ast_type *ret;

	char *name;
	/* parameters */
	int nparam;
	struct ast_variable **param;

	struct ast_block *abstract, 
			 *body;
};

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

struct ast_externdecl {
	enum ast_externdecl_kind {
		EXTERN_FUNCTION = 1 << 0,
		EXTERN_VARIABLE	= 1 << 1,
	} kind;
	union {
		struct ast_function *function;
		struct ast_variable *variable;
	} u;
};

struct ast_externdecl *
ast_functiondecl_create(struct ast_function *);

struct ast_externdecl *
ast_variabledecl_create(struct ast_variable *);

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
