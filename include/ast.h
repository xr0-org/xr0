#ifndef XR0_AST_H
#define XR0_AST_H

#include <stdbool.h>

#include "util.h"

struct ast_expr;

struct ast_expr *
ast_expr_identifier_create(char *);

bool
ast_expr_isidentifier(struct ast_expr *);

char *
ast_expr_as_identifier(struct ast_expr *);

struct ast_expr *
ast_expr_constant_create(int);

struct ast_expr *
ast_expr_constant_create_char(char);

int
ast_expr_as_constant(struct ast_expr *expr);

bool
ast_expr_isconstant(struct ast_expr *);

char *
ast_expr_as_literal(struct ast_expr *);

struct ast_expr *
ast_expr_as_allocation(struct ast_expr *);

struct ast_expr *
ast_expr_literal_create(char *);

struct ast_expr *
ast_expr_bracketed_create(struct ast_expr *);

struct ast_expr *
ast_expr_bracketed_root(struct ast_expr *);

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

bool
ast_expr_incdec_inc(struct ast_expr *);

struct ast_expr *
ast_expr_member_create(struct ast_expr *, char *);

struct ast_expr *
ast_expr_member_root(struct ast_expr *);

char *
ast_expr_member_field(struct ast_expr *);

enum ast_unary_operator;

struct ast_expr *
ast_expr_unary_create(struct ast_expr *, enum ast_unary_operator);

struct ast_expr *
ast_expr_inverted_copy(struct ast_expr *expr, bool invert);

enum ast_unary_operator
ast_expr_unary_op(struct ast_expr *);

bool
ast_expr_unary_isdereference(struct ast_expr *);

bool
ast_expr_isnot(struct ast_expr *);

struct ast_expr *
ast_expr_unary_operand(struct ast_expr *);

struct ast_expr *
ast_expr_eq_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_ne_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_lt_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_gt_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_le_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_ge_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_sum_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_product_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_difference_create(struct ast_expr *, struct ast_expr *);

struct ast_expr *
ast_expr_binary_e1(struct ast_expr *);

struct ast_expr *
ast_expr_binary_e2(struct ast_expr *);

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
ast_expr_isdereferencable_create(struct ast_expr *assertand);

struct ast_expr *
ast_expr_isdereferencable_assertand(struct ast_expr *expr);

bool
ast_expr_isisdereferencable(struct ast_expr *expr);

struct ast_expr *
ast_expr_arbarg_create();

struct ast_expr *
ast_expr_alloc_create(struct ast_expr *);

struct ast_expr *
ast_expr_dealloc_create(struct ast_expr *);

struct ast_expr *
ast_expr_clump_create(struct ast_expr *);

struct ast_expr *
ast_expr_alloc_arg(struct ast_expr *);

enum ast_alloc_kind
ast_expr_alloc_kind(struct ast_expr *);

struct ast_expr *
ast_expr_alloc_kind_create(struct ast_expr *arg, enum ast_alloc_kind);

struct state;

struct error *
ast_expr_alloc_rangeprocess(struct ast_expr *expr, struct ast_expr *lw,
	struct ast_expr *up, struct state *);

void
ast_expr_destroy(struct ast_expr *);

char *
ast_expr_str(struct ast_expr *);

struct ast_expr *
ast_expr_copy(struct ast_expr *);

bool
ast_expr_equal(struct ast_expr *e1, struct ast_expr *e2);

struct math_state;

bool
ast_expr_matheval(struct ast_expr *e);

DECLARE_RESULT_TYPE(int, int, intresult)

struct intresult *
ast_expr_consteval(struct ast_expr *);

bool
ast_expr_decide(struct ast_expr *, struct state *);

bool
ast_expr_rangedecide(struct ast_expr *, struct ast_expr *lw,
		struct ast_expr *up, struct state *);

struct error *
ast_expr_exec(struct ast_expr *, struct state *);

struct e_res;
struct preresult;

bool
preresult_iserror(struct preresult *);

bool
preresult_iscontradiction(struct preresult *);

struct preresult *
ast_expr_assume(struct ast_expr *, struct state *);

void
ast_expr_varinfomap(struct map *, struct ast_expr *, struct state *s);

struct e_res *
ast_expr_eval(struct ast_expr *, struct state *);

struct e_res *
ast_expr_abseval(struct ast_expr *, struct state *);

/* ast_expr_pf_reduce: Reduce an expression to "parameter form", in which its
 * only primitives are constants and parameters (vconsts). */
struct e_res *
ast_expr_pf_reduce(struct ast_expr *, struct state *);

struct value;

struct e_res *
ast_expr_pf_augment(struct value *, struct ast_expr *, struct state *);

struct ast_function;

struct value *
ast_expr_call_arbitrary(struct ast_function *, struct state *);

struct ast_block;
struct lexememarker;

struct ast_expr *
ast_expr_geninstr(struct ast_expr *, struct lexememarker *,
	struct ast_block *, struct state *);

struct ast_stmt;

struct ast_block;
struct ast_variable;

struct ast_block *
ast_block_create(struct ast_stmt **stmt, int nstmt);

void
ast_block_destroy(struct ast_block *);

char *
ast_block_str(struct ast_block *, int indent_level);

char *
ast_block_absstr(struct ast_block *b, int indent_level);

char *
ast_block_render(struct ast_block *, int index);

struct ast_block *
ast_block_copy(struct ast_block *b);

int
ast_block_nstmts(struct ast_block *b);

struct ast_stmt **
ast_block_stmts(struct ast_block *b);

bool
ast_block_empty(struct ast_block *b);

bool
ast_block_isterminal(struct ast_block *, struct state *);

struct ast_type;

struct ast_expr *
ast_block_call_create(struct ast_block *, struct lexememarker *,
	struct ast_type *, struct ast_expr *);

void
ast_block_append_stmt(struct ast_block *, struct ast_stmt *);

struct preconds_result {
	struct ast_block *b;
	struct error *err;
};

struct preconds_result
ast_block_setups(struct ast_block *b, struct state *);

struct ast_stmt;

struct lexememarker;

struct lexememarker *
ast_stmt_lexememarker(struct ast_stmt *);

struct ast_variable_arr;

struct ast_stmt *
ast_stmt_create_declaration(struct lexememarker *, struct ast_variable_arr *);

struct ast_variable_arr *
ast_stmt_declaration_vars(struct ast_stmt *);

bool
ast_stmt_isdecl(struct ast_stmt *);

struct ast_stmt *
ast_stmt_create_labelled(struct lexememarker *, char *label, struct ast_stmt *);

bool
ast_stmt_ispre(struct ast_stmt *);

struct error *
ast_stmt_preconds_validate(struct ast_stmt *);

char *
ast_stmt_labelled_label(struct ast_stmt *);

struct ast_stmt *
ast_stmt_labelled_stmt(struct ast_stmt *);

struct ast_block *
ast_stmt_labelled_as_block(struct ast_stmt *);

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

struct ast_stmt *
ast_stmt_as_compound(struct ast_stmt *);

struct ast_block *
ast_stmt_as_block(struct ast_stmt *);

struct ast_stmt *
ast_stmt_create_compound_v(struct lexememarker *, struct ast_block *);

enum ast_jump_kind {
	JUMP_RETURN	= 1 << 0,
};

struct ast_stmt *
ast_stmt_create_jump(struct lexememarker *, enum ast_jump_kind, struct ast_expr *rv);

struct ast_expr *
ast_stmt_jump_rv(struct ast_stmt *stmt);

struct ast_stmt *
ast_stmt_create_alloc(struct lexememarker *, struct ast_expr *arg);

struct ast_stmt *
ast_stmt_create_dealloc(struct lexememarker *, struct ast_expr *arg);

struct ast_stmt *
ast_stmt_create_clump(struct lexememarker *, struct ast_expr *arg);

struct ast_stmt *
ast_stmt_register_call_create(struct lexememarker *, struct ast_expr *call);

struct ast_stmt *
ast_stmt_register_mov_create(struct lexememarker *, struct ast_variable *v);

struct ast_expr *
ast_stmt_register_call(struct ast_stmt *);

struct ast_variable *
ast_stmt_register_mov(struct ast_stmt *);

bool
ast_stmt_register_iscall(struct ast_stmt *);

void
ast_stmt_destroy(struct ast_stmt *);

struct ast_stmt *
ast_stmt_copy(struct ast_stmt *);

char *
ast_stmt_str(struct ast_stmt *, int indent_level);

bool
ast_stmt_equal(struct ast_stmt *, struct ast_stmt *);

/* ast_stmt_as_block: Wrap in block if single statement. */
struct ast_block *
ast_stmt_as_block(struct ast_stmt *);

struct ast_block *
ast_stmt_as_v_block(struct ast_stmt *);

struct ast_expr *
ast_stmt_as_expr(struct ast_stmt *);

enum execution_mode;

struct error *
ast_stmt_linearise(struct ast_stmt *, struct state *);

bool
ast_stmt_linearisable(struct ast_stmt *);

bool
ast_stmt_isterminal(struct ast_stmt *, struct state *);

bool
ast_stmt_isselection(struct ast_stmt *);

struct error *
ast_stmt_verify(struct ast_stmt *, struct state *);

struct error *
ast_stmt_exec(struct ast_stmt *, struct state *);

struct error *
ast_stmt_process(struct ast_stmt *, struct state *);

struct error *
ast_stmt_absprocess(struct ast_stmt *stmt, struct state *);

struct error *
ast_stmt_absprocess_nosetup(struct ast_stmt *stmt, struct state *);

struct error *
ast_stmt_buildsetup(struct ast_stmt *, struct state *, struct ast_block *);

struct ast_type;

/* TODO: allow modifiers for pointer, array and typedef types */
struct ast_type *
ast_type_create_int();

struct ast_type *
ast_type_create_ptr(struct ast_type *type);

struct ast_type *
ast_type_create_char();

struct ast_type *
ast_type_create_void();

struct ast_type *
ast_type_create_voidptr();

struct ast_type *
ast_type_create_arr(struct ast_type *type, int length);

bool
ast_type_isarr(struct ast_type *);

bool
ast_type_isptr(struct ast_type *);

struct ast_type *
ast_type_arr_type(struct ast_type *);

struct ast_variable_arr;

struct ast_type *
ast_type_create_struct(char *tag, struct ast_variable_arr *);

struct externals;

struct ast_type *
ast_type_struct_complete(struct ast_type *t, struct externals *ext);

struct ast_variable_arr *
ast_type_struct_members(struct ast_type *t);

struct ast_type *
ast_type_struct_membertype(struct ast_type *t, char *field, struct externals *);

char *
ast_type_struct_tag(struct ast_type *t);

struct ast_type *
ast_type_create_struct_anonym(struct ast_variable_arr *);

struct ast_type *
ast_type_create_struct_partial(char *tag);

struct ast_type *
ast_type_create_userdef(char *name);

bool
ast_type_isstruct(struct ast_type *);

bool
ast_type_istypedef(struct ast_type *);

bool
ast_type_isvoid(struct ast_type *);

struct ast_type *
ast_type_create_range(struct ast_expr *lw, struct ast_expr *up_nonincl);

struct externals;

struct value *
ast_type_vconst(struct ast_type *, struct state *s, char *key, bool persist);

void
ast_type_destroy(struct ast_type *);

char *
ast_type_str(struct ast_type *);

char *
ast_type_strwithvar(struct ast_type *, char *var);

struct ast_type *
ast_type_copy(struct ast_type *t);

enum ast_type_base
ast_type_base(struct ast_type *t);

struct ast_type *
ast_type_ptr_type(struct ast_type *t);

int
ast_type_size(struct ast_type *);

struct ast_type *
ast_type_deref(struct ast_type *);

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
ast_variable_arr_copy(struct ast_variable_arr *);

struct ast_variable;

struct ast_variable *
ast_variable_create(char *name, struct ast_type *);

void
ast_variable_setinit(struct ast_variable *, struct ast_expr *);

struct ast_expr *
ast_variable_init(struct ast_variable *);

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

void
ast_function_setname(struct ast_function *f, char *name);

char *
ast_function_name(struct ast_function *f);

struct ast_function *
ast_function_copy(struct ast_function *);

bool
ast_function_isaxiom(struct ast_function *f);

bool
ast_function_isproto(struct ast_function *f);

bool
ast_function_absisempty(struct ast_function *f);

bool
ast_function_isvoid(struct ast_function *f);

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

struct error *
ast_function_initparams(struct ast_function *, struct state *);

struct error *
ast_function_initsetup(struct ast_function *, struct state *);

struct externals;
struct error;

struct error *
ast_function_verify(struct ast_function *, struct externals *);

struct error *
ast_function_debug(struct ast_function *, struct externals *);

struct ast_externdecl;

struct ast_externdecl *
ast_functiondecl_create(struct ast_function *);

bool
ast_externdecl_isfunction(struct ast_externdecl *);

struct ast_function *
ast_externdecl_as_function(struct ast_externdecl *);

struct ast_externdecl *
ast_decl_create(char *name, struct ast_type *);

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

struct string_arr *
ast_topological_order(char *fname, struct externals *ext);

struct ast_function *
ast_protostitch(struct ast_function *, struct externals *);

#endif
