#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "intern.h"
#include "ext.h"
#include "expr.h"
#include "math.h"
#include "state.h"
#include "stmt/stmt.h"
#include "util.h"
#include "value.h"

static struct ast_expr *
ast_expr_create()
{
	return malloc(sizeof(struct ast_expr));
}

struct ast_expr *
ast_expr_identifier_create(char *s)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_IDENTIFIER;
	expr->u.string = s;
	return expr;
}

bool
ast_expr_isidentifier(struct ast_expr *expr)
{
	return expr->kind == EXPR_IDENTIFIER;
}

char *
ast_expr_as_identifier(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_IDENTIFIER);
	return expr->u.string;
}

static void
ast_expr_destroy_identifier(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_IDENTIFIER);
	free(expr->u.string);
}

struct ast_expr *
ast_expr_constant_create(int k)
{
	/* TODO: generalise for all constant cases */
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_CONSTANT;
	expr->u.constant.ischar = false;
	expr->u.constant.constant = k;
	return expr;
}

struct ast_expr *
ast_expr_constant_create_char(char c)
{
	/* TODO: generalise for all constant cases */
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_CONSTANT;
	expr->u.constant.ischar = true;
	expr->u.constant.constant = c;
	return expr;
}

static char *
escape_str(char c);

static void
ast_expr_constant_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	int constant = expr->u.constant.constant;
	if (!expr->u.constant.ischar) {
		strbuilder_printf(b, "%d", constant);
		return;
	}
	switch (constant) {
	case '\n': case '\t': case '\v': case '\b': case '\f':
	case '\a': case '\\': case '\?': case '\'': case '\"':
	case '\0':
		strbuilder_printf(b, "'%s'", escape_str(constant));
		break;
	default:
		strbuilder_printf(b, "'%c'", constant);
		break;
	}
}

static char *
escape_str(char c)
{
	switch (c) { 
	case '\n': return "\\n";
	case '\t': return "\\t";
	case '\v': return "\\v";
	case '\b': return "\\b";
	case '\f': return "\\f";
	case '\a': return "\\a";
	case '\\': return "\\\\";
	case '\?': return "\\?";
	case '\'': return "\\\'";
	case '\"': return "\\\"";
	/* TODO: handle octal and hex escapes */
	case '\0': return "\\0";
	default: assert(false);
	}
}

int
ast_expr_as_constant(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_CONSTANT);
	return expr->u.constant.constant;
}

bool
ast_expr_isconstant(struct ast_expr *expr)
{
	return expr->kind == EXPR_CONSTANT;
}



struct ast_expr *
ast_expr_literal_create(char *s)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_STRING_LITERAL;
	expr->u.string = s;
	return expr;
}

char *
ast_expr_as_literal(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_STRING_LITERAL);
	return expr->u.string;
}

static void
ast_expr_destroy_literal(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_STRING_LITERAL);
	free(expr->u.string);
}

struct ast_expr *
ast_expr_bracketed_create(struct ast_expr *root)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_BRACKETED;
	expr->root = root;
	return expr;
}

struct ast_expr *
ast_expr_bracketed_root(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_BRACKETED);
	return expr->root;
}

static void
ast_expr_bracketed_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	char *root = ast_expr_str(expr->root);
	strbuilder_printf(b, "(%s)", root);
	free(root);
}


struct ast_expr *
ast_expr_iteration_create()
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_ITERATION;
	return expr;
}

struct ast_expr *
ast_expr_call_create(struct ast_expr *root, int narg, struct ast_expr **arg)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_CALL;
	expr->root = root;
	expr->u.call.n = narg;
	expr->u.call.arg = arg;
	return expr;
}

struct ast_expr *
ast_expr_call_root(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_CALL);
	return expr->root;
}

int
ast_expr_call_nargs(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_CALL);
	return expr->u.call.n;
}

struct ast_expr **
ast_expr_call_args(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_CALL);
	return expr->u.call.arg;
}

static void
ast_expr_call_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	char *root = ast_expr_str(expr->root);
	strbuilder_printf(b, "%s(", root);
	for (int i = 0; i < expr->u.call.n; i++) {
		char *arg = ast_expr_str(expr->u.call.arg[i]);
		strbuilder_printf(b, "%s%s", arg,
			(i + 1 < expr->u.call.n) ? ", " : "");
		free(arg);
	}
	strbuilder_printf(b, ")");
	free(root);
}

static void
ast_expr_destroy_call(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_CALL);
	ast_expr_destroy(expr->root);
	for (int i = 0; i < expr->u.call.n; i++) {
		ast_expr_destroy(expr->u.call.arg[i]);
	}
	free(expr->u.call.arg);
}

static struct ast_expr *
ast_expr_copy_call(struct ast_expr *expr)
{
	struct ast_expr **arg = malloc(sizeof(struct ast_expr *) * expr->u.call.n);
	for (int i = 0; i < expr->u.call.n; i++) {
		arg[i] = ast_expr_copy(expr->u.call.arg[i]);
	}
	return ast_expr_call_create(
		ast_expr_copy(expr->root),
		expr->u.call.n,
		arg
	);
}

struct ast_expr *
ast_expr_incdec_create(struct ast_expr *root, bool inc, bool pre)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_INCDEC;
	expr->root = root;
	expr->u.incdec.inc = inc;
	expr->u.incdec.pre = pre;
	return expr;
}

struct ast_expr *
ast_expr_incdec_to_assignment(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_INCDEC);

	return ast_expr_assignment_create(
		ast_expr_copy(expr->root),
		ast_expr_binary_create(
			ast_expr_copy(expr->root),
			expr->u.incdec.inc ? BINARY_OP_ADDITION : BINARY_OP_SUBTRACTION,
			ast_expr_constant_create(1)
		)
	);
}

bool
ast_expr_incdec_pre(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_INCDEC);

	return expr->u.incdec.pre;
}

bool
ast_expr_incdec_inc(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_INCDEC);

	return expr->u.incdec.inc;
}

struct ast_expr *
ast_expr_incdec_root(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_INCDEC);

	return expr->root;
}

static void
ast_expr_destroy_incdec(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_INCDEC);
	ast_expr_destroy(expr->root);
}

static void
ast_expr_incdec_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	char *root = ast_expr_str(expr->root);
	char *op = expr->u.incdec.inc ? "++" : "--";
	if (expr->u.incdec.pre) {
		strbuilder_printf(b, "%s%s", op, root);
	} else {
		strbuilder_printf(b, "%s%s", root, op);
	}
	free(root);
}

struct ast_expr *
ast_expr_member_create(struct ast_expr *_struct, char *field)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_STRUCTMEMBER;
	expr->root = _struct;
	expr->u.string = field;
	return expr;
}

struct ast_expr *
ast_expr_member_root(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_STRUCTMEMBER);
	return expr->root;
}

char *
ast_expr_member_field(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_STRUCTMEMBER);
	return expr->u.string;
}

static void
ast_expr_member_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	char *r = ast_expr_str(expr->root);
	strbuilder_printf(b, "%s.%s", r, expr->u.string);
	free(r);
}

struct ast_expr *
ast_expr_unary_create(struct ast_expr *root, enum ast_unary_operator op)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_UNARY;
	expr->root = root;
	expr->u.unary_op = op;
	return expr;
}

struct ast_expr *
ast_expr_inverted_copy(struct ast_expr *expr, bool invert)
{
	struct ast_expr *copy = ast_expr_copy(expr);
	return invert
		? ast_expr_unary_create(copy, UNARY_OP_BANG)
		: copy;
}

static void
ast_expr_destroy_unary(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_UNARY);

	ast_expr_destroy(expr->root);
}

enum ast_unary_operator
ast_expr_unary_op(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_UNARY);

	return expr->u.unary_op;
}

struct ast_expr *
ast_expr_unary_operand(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_UNARY);

	return expr->root;
}

bool
ast_expr_isverifiable(struct ast_expr *expr)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
	case EXPR_CONSTANT:
		return true;
	case EXPR_STRUCTMEMBER:
		return ast_expr_isverifiable(ast_expr_member_root(expr));
	case EXPR_BRACKETED:
		return ast_expr_isverifiable(ast_expr_bracketed_root(expr));
	case EXPR_UNARY:
		return ast_expr_isverifiable(ast_expr_unary_operand(expr));
	case EXPR_ISDEALLOCAND:
		return ast_expr_isverifiable(ast_expr_isdeallocand_assertand(expr));
	case EXPR_INCDEC:
	case EXPR_CALL:
	case EXPR_ASSIGNMENT:
	case EXPR_RANGE:
		return false;
	case EXPR_BINARY:
		return ast_expr_isverifiable(ast_expr_binary_e1(expr)) &&
			ast_expr_isverifiable(ast_expr_binary_e2(expr));
	default:
		assert(false);
	}
}

bool
ast_expr_unary_isdereference(struct ast_expr *expr)
{
	if (ast_expr_kind(expr) != EXPR_UNARY) {
		return false;
	}
	return ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE;
}

bool
ast_expr_isnot(struct ast_expr *expr)
{
	return ast_expr_kind(expr) == EXPR_UNARY &&
		ast_expr_unary_op(expr) == UNARY_OP_BANG;
}

static void
ast_expr_unary_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	enum ast_unary_operator op = expr->u.unary_op;
	assert(UNARY_OP_ADDRESS <= op && op <= UNARY_OP_BANG);

	const char opchar[] = {
		[UNARY_OP_ADDRESS]		= '&',
		[UNARY_OP_DEREFERENCE]		= '*',
		[UNARY_OP_POSITIVE]		= '+',
		[UNARY_OP_NEGATIVE]		= '-',
		[UNARY_OP_ONES_COMPLEMENT]	= '~',
		[UNARY_OP_BANG]			= '!',
	};

	char *root = ast_expr_str(expr->root);
	strbuilder_printf(b, "%c(%s)", opchar[op], root);
	free(root);
}

struct ast_expr *
ast_expr_binary_create(struct ast_expr *e1, enum ast_binary_operator op,
		struct ast_expr *e2)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_BINARY;
	expr->u.binary.e1 = e1;
	expr->u.binary.op = op;
	expr->u.binary.e2 = e2;
	return expr;
}

struct ast_expr *
ast_expr_eq_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_EQ, e2);
}

struct ast_expr *
ast_expr_ne_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_NE, e2);
}

struct ast_expr *
ast_expr_lt_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_LT, e2);
}

struct ast_expr *
ast_expr_gt_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_GT, e2);
}

struct ast_expr *
ast_expr_le_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_LE, e2);
}

struct ast_expr *
ast_expr_ge_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_GE, e2);
}

struct ast_expr *
ast_expr_sum_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_ADDITION, e2);
}

struct ast_expr *
ast_expr_product_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_MULTIPLICATION, e2);
}

struct ast_expr *
ast_expr_difference_create(struct ast_expr *e1, struct ast_expr *e2)
{
	return ast_expr_binary_create(e1, BINARY_OP_SUBTRACTION, e2);
}

struct ast_expr *
ast_expr_binary_e1(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_BINARY);
	return expr->u.binary.e1;
}

struct ast_expr *
ast_expr_binary_e2(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_BINARY);
	return expr->u.binary.e2;
}

enum ast_binary_operator
ast_expr_binary_op(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_BINARY);
	return expr->u.binary.op;
}

static void
ast_expr_destroy_binary(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_BINARY);
	ast_expr_destroy(expr->u.binary.e1);
	ast_expr_destroy(expr->u.binary.e2);
}

static struct math_expr *
math_expr(struct ast_expr *);

static char *
atomicstr(struct ast_expr *);

static void
ast_expr_binary_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	const char *opstr[] = {
		[BINARY_OP_EQ]			= "==",
		[BINARY_OP_NE]			= "!=",

		[BINARY_OP_LT]			= "<",
		[BINARY_OP_GT]			= ">",
		[BINARY_OP_LE]			= "<=",
		[BINARY_OP_GE]			= ">=",

		[BINARY_OP_ADDITION]		= "+",
		[BINARY_OP_SUBTRACTION]		= "-",
		[BINARY_OP_MULTIPLICATION]	= "*",
	};
	char *e1 = atomicstr(expr->u.binary.e1),
	     *e2 = atomicstr(expr->u.binary.e2);
	strbuilder_printf(b, "%s%s%s", e1, opstr[expr->u.binary.op], e2);
	free(e1);
	free(e2);
}

static bool
isatomic(struct ast_expr *);

static char *
atomicstr(struct ast_expr *e)
{
	if (isatomic(e)) {
		return ast_expr_str(e);
	}
	struct strbuilder *b = strbuilder_create();
	char *s = ast_expr_str(e);
	strbuilder_printf(b, "(%s)", s);
	free(s);
	return strbuilder_build(b);
}

static bool
isatomic(struct ast_expr *e)
{
	switch (e->kind) {
	case EXPR_IDENTIFIER:
	case EXPR_CONSTANT:
	case EXPR_BRACKETED:
	case EXPR_CALL:
	case EXPR_INCDEC:
	case EXPR_STRUCTMEMBER:
	case EXPR_UNARY:
	case EXPR_ISDEALLOCAND:
	case EXPR_RANGE:
		return true;
	case EXPR_BINARY:
	case EXPR_ASSIGNMENT:
		return false;
	default:
		assert(false);
	}
}

struct ast_expr *
ast_expr_assignment_create(struct ast_expr *root, struct ast_expr *value)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_ASSIGNMENT;
	expr->root = root;
	expr->u.assignment_value = value;
	return expr;
}


struct ast_expr *
ast_expr_assignment_lval(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_ASSIGNMENT);
	return expr->root;
}

struct ast_expr *
ast_expr_assignment_rval(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_ASSIGNMENT);
	return expr->u.assignment_value;
}

static void
ast_expr_destroy_assignment(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_ASSIGNMENT);
	ast_expr_destroy(expr->root);
	ast_expr_destroy(expr->u.assignment_value);
}

static void
ast_expr_assignment_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	char *root = ast_expr_str(expr->root),
	     *value = ast_expr_str(expr->u.assignment_value);
	strbuilder_printf(b, "%s = %s", root, value);
	free(value);
	free(root);
}

struct ast_expr *
ast_expr_isdeallocand_create(struct ast_expr *assertand)
{
	struct ast_expr *new = ast_expr_create();
	new->kind = EXPR_ISDEALLOCAND;
	new->root = assertand;
	return new;
}

struct ast_expr *
ast_expr_isdeallocand_assertand(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_ISDEALLOCAND);
	return expr->root;
}

static void
ast_expr_isdeallocand_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	char *root = ast_expr_str(expr->root);
	strbuilder_printf(b, "@%s", root);
	free(root);
}

struct ast_expr *
ast_expr_range_createnokey(struct ast_expr *lw, struct ast_expr *up)
{
	assert(lw && up);

	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_RANGE;
	expr->u.range.key = NULL;
	expr->u.range.lw = lw;
	expr->u.range.up = up;
	return expr;
}

struct ast_expr *
ast_expr_range_create(char *key, struct ast_expr *lw, struct ast_expr *up)
{
	assert(key && lw && up);

	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_RANGE;
	expr->u.range.key = key;
	expr->u.range.lw = lw;
	expr->u.range.up = up;
	return expr;
}

static void
ast_expr_range_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	char *lw = ast_expr_str(expr->u.range.lw),
	     *up = ast_expr_str(expr->u.range.up);
	if (ast_expr_range_haskey(expr)) {
		strbuilder_printf(b, "$%s[%s?%s]", expr->u.range.key, lw, up);
	} else {
		strbuilder_printf(b, "[%s!%s]", lw, up);
	}
	free(up);
	free(lw);
}

int
ast_expr_range_haskey(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_RANGE);
	return (long) expr->u.range.key;
}

char *
ast_expr_range_key(struct ast_expr *expr)
{
	assert(ast_expr_range_haskey(expr));
	return expr->u.range.key;
}

struct ast_expr *
ast_expr_range_lw(struct ast_expr *e)
{
	assert(e->kind == EXPR_RANGE);
	return e->u.range.lw;
}

struct ast_expr *
ast_expr_range_up(struct ast_expr *e)
{
	assert(e->kind == EXPR_RANGE);
	return e->u.range.up;
}


struct ast_expr *
ast_expr_rangemin_create()
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_RANGEBOUND;
	expr->u.ismax = false;
	return expr;
}

struct ast_expr *
ast_expr_rangemax_create()
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_RANGEBOUND;
	expr->u.ismax = true;
	return expr;
}



bool
ast_expr_israngemin(struct ast_expr *e)
{
	return e->kind == EXPR_RANGEBOUND && !e->u.ismax;
}

bool
ast_expr_israngemax(struct ast_expr *e)
{
	return e->kind == EXPR_RANGEBOUND && e->u.ismax;
}


struct ast_expr *
ast_expr_alloc_create(struct ast_expr *arg)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_ALLOCATION;
	expr->u.alloc.kind = ALLOC;
	expr->u.alloc.arg = arg;
	return expr;
}

struct ast_expr *
ast_expr_dealloc_create(struct ast_expr *arg)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_ALLOCATION;
	expr->u.alloc.kind = DEALLOC;
	expr->u.alloc.arg = arg;
	return expr;
}

struct ast_expr *
ast_expr_clump_create(struct ast_expr *arg)
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_ALLOCATION;
	expr->u.alloc.kind = CLUMP;
	expr->u.alloc.arg = arg;
	return expr;
}

struct ast_expr *
ast_expr_alloc_kind_create(struct ast_expr *arg, enum ast_alloc_kind kind)
{
	switch (kind) {
	case ALLOC:
		return ast_expr_alloc_create(arg);
	case DEALLOC:
		return ast_expr_dealloc_create(arg);
	case CLUMP:
		return ast_expr_clump_create(arg);
	default:
		assert(false);
	}
}

static struct ast_expr *
ast_expr_alloc_copy(struct ast_expr *expr)
{
	struct ast_expr *arg = ast_expr_copy(expr->u.alloc.arg);
	switch (expr->u.alloc.kind) {
	case ALLOC:
		return ast_expr_alloc_create(arg);
	case DEALLOC:
		return ast_expr_dealloc_create(arg);
	case CLUMP:
		return ast_expr_clump_create(arg);
	default:
		assert(false);
	}
}

static void
ast_expr_alloc_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	assert(expr->kind == EXPR_ALLOCATION);

	char *arg = ast_expr_str(expr->u.alloc.arg);

	switch (expr->u.alloc.kind) {
	case ALLOC:
		strbuilder_printf(b, ".%s(%s)", "malloc", arg);
		break;
	case DEALLOC:
		strbuilder_printf(b, ".%s(%s)", "free", arg);
		break;
	case CLUMP:
		strbuilder_printf(b, ".%s(%s)", "clump", arg);
		break;
	default:
		assert(false);
	}
	free(arg);
}

struct ast_expr *
ast_expr_alloc_arg(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_ALLOCATION);
	return expr->u.alloc.arg;
}

enum ast_alloc_kind
ast_expr_alloc_kind(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_ALLOCATION);
	return expr->u.alloc.kind;
}

void
ast_expr_destroy(struct ast_expr *expr)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
		ast_expr_destroy_identifier(expr);
		break;
	case EXPR_STRING_LITERAL:
		ast_expr_destroy_literal(expr);
		break;
	case EXPR_BRACKETED:
		ast_expr_destroy(expr->root);
		break;
	case EXPR_CALL:
		ast_expr_destroy_call(expr);
		break;
	case EXPR_INCDEC:
		ast_expr_destroy_incdec(expr);
		break;
	case EXPR_STRUCTMEMBER:
		ast_expr_destroy(expr->root); free(expr->u.string);
		break;
	case EXPR_UNARY:
		ast_expr_destroy_unary(expr);
		break;
	case EXPR_BINARY:
		ast_expr_destroy_binary(expr);
		break;
	case EXPR_ASSIGNMENT:
		ast_expr_destroy_assignment(expr);
		break;
	case EXPR_CONSTANT:
		break;
	case EXPR_ISDEALLOCAND:
		ast_expr_destroy(expr->root);
		break;
	case EXPR_RANGE:
		if (expr->u.range.key) {
			free(expr->u.range.key);
		}
		if (expr->u.range.lw) {
			ast_expr_destroy(expr->u.range.lw);
		}
		if (expr->u.range.up) {
			ast_expr_destroy(expr->u.range.up);
		}
		break;
	case EXPR_RANGEBOUND:
		break;
	case EXPR_ALLOCATION:
		ast_expr_destroy(expr->u.alloc.arg);
		break;
	default:
		assert(false);
	}
	free(expr);
}

char *
ast_expr_str(struct ast_expr *expr)
{
	struct strbuilder *b = strbuilder_create();
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
		strbuilder_printf(b, expr->u.string);
		break;
	case EXPR_CONSTANT:
		ast_expr_constant_str_build(expr, b);
		break;
	case EXPR_STRING_LITERAL:
		strbuilder_printf(b, "\"%s\"", expr->u.string);
		break;
	case EXPR_BRACKETED:
		ast_expr_bracketed_str_build(expr, b);
		break;
	case EXPR_CALL:
		ast_expr_call_str_build(expr, b);
		break;
	case EXPR_INCDEC:
		ast_expr_incdec_str_build(expr, b);
		break;
	case EXPR_STRUCTMEMBER:
		ast_expr_member_str_build(expr, b);
		break;
	case EXPR_UNARY:
		ast_expr_unary_str_build(expr, b);
		break;
	case EXPR_BINARY:
		ast_expr_binary_str_build(expr, b);
		break;
	case EXPR_ASSIGNMENT:
		ast_expr_assignment_str_build(expr, b);
		break;
	case EXPR_ISDEALLOCAND:
		ast_expr_isdeallocand_str_build(expr, b);
		break;
	case EXPR_RANGE:
		ast_expr_range_str_build(expr, b);
		break;
	case EXPR_RANGEBOUND:
		strbuilder_printf(b, expr->u.ismax ? "MAX" : "MIN");
		break;
	case EXPR_ALLOCATION:
		ast_expr_alloc_str_build(expr, b);
		break;
	default:
		assert(false);
	}
	return strbuilder_build(b);
}

struct ast_expr *
ast_expr_copy(struct ast_expr *expr)
{
	assert(expr);

	switch (expr->kind) {
	case EXPR_IDENTIFIER:
		return ast_expr_identifier_create(dynamic_str(expr->u.string));
	case EXPR_CONSTANT:
		return expr->u.constant.ischar
			? ast_expr_constant_create_char(expr->u.constant.constant)
			: ast_expr_constant_create(expr->u.constant.constant);
	case EXPR_STRING_LITERAL:
		return ast_expr_literal_create(dynamic_str(expr->u.string));
	case EXPR_BRACKETED:
		return ast_expr_bracketed_create(ast_expr_copy(expr->root));
	case EXPR_CALL:
		return ast_expr_copy_call(expr);
	case EXPR_INCDEC:
		return ast_expr_incdec_create(
			ast_expr_copy(expr->root),
			expr->u.incdec.inc,
			expr->u.incdec.pre
		);
	case EXPR_STRUCTMEMBER:
		return ast_expr_member_create(
			ast_expr_copy(expr->root), dynamic_str(expr->u.string)
		);
	case EXPR_UNARY:
		return ast_expr_unary_create(
			ast_expr_copy(expr->root),
			expr->u.unary_op
		);
	case EXPR_BINARY:
		return ast_expr_binary_create(
			ast_expr_copy(expr->u.binary.e1),
			expr->u.binary.op,
			ast_expr_copy(expr->u.binary.e2)
		);
	case EXPR_ASSIGNMENT:
		return ast_expr_assignment_create(
			ast_expr_copy(expr->root),
			ast_expr_copy(expr->u.assignment_value)
		);
	case EXPR_ISDEALLOCAND:
		return ast_expr_isdeallocand_create(
			ast_expr_copy(expr->root)
		);
	case EXPR_RANGE:
		if (ast_expr_range_haskey(expr)) {
			return ast_expr_range_create(
				dynamic_str(expr->u.range.key),
				ast_expr_copy(expr->u.range.lw),
				ast_expr_copy(expr->u.range.up)
			);
		}
		return ast_expr_range_createnokey(
			ast_expr_copy(expr->u.range.lw),
			ast_expr_copy(expr->u.range.up)
		);
	case EXPR_RANGEBOUND:
		return expr->u.ismax
			? ast_expr_rangemax_create()
			: ast_expr_rangemin_create();
	case EXPR_ALLOCATION:
		return ast_expr_alloc_copy(expr);
	default:
		assert(false);
	}
}

enum ast_expr_kind
ast_expr_kind(struct ast_expr *expr)
{
	return expr->kind;
}

bool
ast_expr_equal(struct ast_expr *e1, struct ast_expr *e2)
{
	if (!e1 || !e2) {
		return false;
	}
	if (e1->kind != e2->kind) {
		return false;	
	}
	switch (e1->kind) {
	case EXPR_CONSTANT:
		return e1->u.constant.constant == e2->u.constant.constant;
	case EXPR_IDENTIFIER:
		return strcmp(ast_expr_as_identifier(e1), ast_expr_as_identifier(e2)) == 0;
	case EXPR_STRING_LITERAL:
		return strcmp(ast_expr_as_literal(e1), ast_expr_as_literal(e2)) == 0;
	case EXPR_ASSIGNMENT:
		return ast_expr_equal(e1->root, e2->root)
			&& ast_expr_equal(e1->u.assignment_value, e2->u.assignment_value); 
	case EXPR_UNARY:
		return e1->u.unary_op == e2->u.unary_op &&
			ast_expr_equal(e1->root, e2->root);

	case EXPR_BINARY:
		return ast_expr_binary_op(e1) == ast_expr_binary_op(e2) &&
			ast_expr_equal(ast_expr_binary_e1(e1), ast_expr_binary_e1(e2)) && 
			ast_expr_equal(ast_expr_binary_e2(e1), ast_expr_binary_e2(e2));
	case EXPR_CALL:
		if (e1->u.call.n != e2->u.call.n) {
			return false;
		}
		for (int i = 0; i < e1->u.call.n; i++) {
			if (!ast_expr_equal(e1->u.call.arg[i], e2->u.call.arg[i])) {
				return false;
			}
		}
		return ast_expr_equal(e1->root, e2->root);
	case EXPR_STRUCTMEMBER:
		return ast_expr_equal(
			ast_expr_member_root(e1),
			ast_expr_member_root(e2)
		) && (strcmp(
			ast_expr_member_field(e1), ast_expr_member_field(e2)
		) == 0);
	case EXPR_ALLOCATION:
		return ast_expr_alloc_kind(e1) == ast_expr_alloc_kind(e2) && 
			ast_expr_equal(ast_expr_alloc_arg(e1), ast_expr_alloc_arg(e2));
	default:
		assert(false);
	}
}

static bool
eval_prop(struct math_expr *e1, enum ast_binary_operator, struct math_expr *e2);

bool
ast_expr_matheval(struct ast_expr *e)
{
	assert(e->kind == EXPR_BINARY);

	struct math_expr *e1 = math_expr(e->u.binary.e1),
			 *e2 = math_expr(e->u.binary.e2);

	bool val = eval_prop(e1, e->u.binary.op, e2);

	math_expr_destroy(e2);
	math_expr_destroy(e1);

	return val;
}

static bool
eval_prop(struct math_expr *e1, enum ast_binary_operator op, struct math_expr *e2)
{
	switch (op) {
	case BINARY_OP_EQ:
		return math_eq(e1, e2);
	case BINARY_OP_NE:
		return !math_eq(e1, e2);
	case BINARY_OP_LT:
		return math_lt(e1, e2);
	case BINARY_OP_GT:
		return math_gt(e1, e2);
	case BINARY_OP_LE:
		return math_le(e1, e2);
	case BINARY_OP_GE:
		return math_ge(e1, e2);
	default:
		assert(false);
	}
}

static struct math_expr *
binary_e2(struct ast_expr *e2, enum ast_binary_operator op);

static struct math_expr *
math_expr(struct ast_expr *e)
{
	switch (e->kind) {
	case EXPR_IDENTIFIER:
		return math_expr_atom_create(
			math_atom_variable_create(dynamic_str(e->u.string))
		);
	case EXPR_CONSTANT:
		if (e->u.constant.constant < 0) {
			return math_expr_neg_create(
				math_expr_atom_create(
					math_atom_nat_create(-e->u.constant.constant)
				)
			);
		}
		return math_expr_atom_create(
			math_atom_nat_create(e->u.constant.constant)
		);
	case EXPR_BINARY:
		return math_expr_sum_create(
			math_expr(e->u.binary.e1),
			binary_e2(e->u.binary.e2, e->u.binary.op)
		);
	default:
		assert(false);
	}
}

static struct math_expr *
binary_e2(struct ast_expr *e2, enum ast_binary_operator op)
{
	switch (op) {
	case BINARY_OP_ADDITION:
		return math_expr(e2);
	case BINARY_OP_SUBTRACTION:
		return math_expr_neg_create(math_expr(e2));
	default:
		assert(false);
	}
}

struct tagval {
	char *tag;
	struct value *v;
};

static struct tagval *
tagval_value_create(struct value *v)
{
	struct tagval *tv = malloc(sizeof(struct tagval));
	tv->tag = NULL;
	tv->v = v;
	return tv;
}

static struct tagval *
tagval_tagged_create(struct value *v, char *tag)
{
	assert(tag);

	struct tagval *tv = malloc(sizeof(struct tagval));
	tv->tag = tag;
	tv->v = v;
	return tv;
}

bool
tagval_hastag(struct tagval *tv)
{
	return tv->tag;
}

char *
tagval_tag(struct tagval *tv)
{
	assert(tagval_hastag(tv));
	return tv->tag;
}

struct value *
tagval_value(struct tagval *tv)
{
	return tv->v;
}

void
tagval_destroy(struct tagval *tv)
{
	if (tv->tag) {
		free(tv->tag);
	}
	value_destroy(tv->v);
	free(tv);
}

DEFINE_RESULT_TYPE(struct tagval *, tagval, tagval_destroy, tagval_res, false)

static struct tagval_res *
binary_rangeeval(struct ast_expr *, struct state *);

struct tagval_res *
ast_expr_rangeeval(struct ast_expr *e, struct state *s)
{
	switch (e->kind) {
	case EXPR_CONSTANT:
		return tagval_res_tagval_create(
			tagval_value_create(
				value_int_create(ast_expr_as_constant(e))
			)
		);
	case EXPR_IDENTIFIER:
		return tagval_res_tagval_create(
			tagval_tagged_create(
				state_getrconst(s, ast_expr_as_identifier(e)),
				dynamic_str(ast_expr_as_identifier(e))
			)
		);
	case EXPR_BINARY:
		return binary_rangeeval(e, s);
	default:
		assert(false);
	}
}

static struct tagval_res *
binary_rangeeval_actual(struct tagval *, enum ast_binary_operator,
		struct tagval *);

static struct tagval_res *
binary_rangeeval(struct ast_expr *e, struct state *s)
{
	struct tagval_res *vres1 = ast_expr_rangeeval(ast_expr_binary_e1(e), s),
			  *vres2 = ast_expr_rangeeval(ast_expr_binary_e2(e), s);
	if (tagval_res_iserror(vres1)) {
		return vres1;
	}
	if (tagval_res_iserror(vres2)) {
		return vres2;
	}
	return binary_rangeeval_actual(
		tagval_res_as_tagval(vres1),
		ast_expr_binary_op(e),
		tagval_res_as_tagval(vres2)
	);
}

static struct value *
binary_rangeeval_norm(struct value *, enum ast_binary_operator, int);

static struct tagval *
addtagifpresent(struct value *v, struct tagval *);

static struct tagval_res *
binary_rangeeval_actual(struct tagval *tv1, enum ast_binary_operator op,
		struct tagval *tv2)
{
	struct value *v1 = tagval_value(tv1),
		     *v2 = tagval_value(tv2);
	if (!value_isconstant(v2)) {
		if (!value_isconstant(v1)){ 
			return tagval_res_error_create(
				error_printf("one must be constant")
			); 
		}
		return binary_rangeeval_actual(tv2, op, tv1);
	}
	/* ⊢ value_isconstant(v2) */
	return tagval_res_tagval_create(
		addtagifpresent(
			binary_rangeeval_norm(v1, op, value_as_constant(v2)),
			tv1
		)
	);
}

static struct value *
binary_rangeeval_norm(struct value *v, enum ast_binary_operator op, int c)
{
	int lw = value_int_lw(v),
	    up = value_int_up(v);
	assert(c >= 0);

	switch (op) {
	case BINARY_OP_ADDITION:
		return value_int_range_create(lw+c, up+c);
	case BINARY_OP_SUBTRACTION:
		return value_int_range_create(lw-c, up-c);
	case BINARY_OP_MULTIPLICATION:
		if (!value_isconstant(v)) {
			/* XXX: cannot multiply ranges without modulo-ranges */
			assert(c == 1);
			return v;
		}
		/* ⊢ lw == value_as_constant(v) */
		return value_int_create(lw*c);
	default:
		assert(false);
	}
}

static struct tagval *
addtagifpresent(struct value *v, struct tagval *tv)
{
	return tagval_hastag(tv)
		? tagval_tagged_create(v, tagval_tag(tv))
		: tagval_value_create(v);
}

static struct string_arr *
ast_expr_range_getfuncs(struct ast_expr *);

static struct string_arr *
ast_expr_call_getfuncs(struct ast_expr *);

struct string_arr *
ast_expr_getfuncs(struct ast_expr *expr)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
	case EXPR_CONSTANT:
	case EXPR_STRING_LITERAL:
	case EXPR_STRUCTMEMBER:
	case EXPR_ISDEALLOCAND:
	case EXPR_RANGEBOUND:
		return string_arr_create();
	case EXPR_RANGE:
		return ast_expr_range_getfuncs(expr);
	case EXPR_CALL:
		return ast_expr_call_getfuncs(expr);
	case EXPR_BRACKETED:
	case EXPR_UNARY:
	case EXPR_INCDEC:
		return ast_expr_getfuncs(expr->root);
	case EXPR_ASSIGNMENT:
		return string_arr_concat(
			ast_expr_getfuncs(expr->root),
			ast_expr_getfuncs(expr->u.assignment_value)
		);
	case EXPR_BINARY:
		return string_arr_concat(
			ast_expr_getfuncs(expr->u.binary.e1),
			ast_expr_getfuncs(expr->u.binary.e2)
		);
	default:
		assert(false);
	}
}

static struct string_arr *
ast_expr_call_getfuncs(struct ast_expr *expr)
{
	struct string_arr *res = string_arr_create();
	struct ast_expr *root = expr->root;
	assert(root->kind == EXPR_IDENTIFIER);
	string_arr_append(res, dynamic_str(root->u.string));
	for (int i = 0; i < expr->u.call.n; i++) {
		res = string_arr_concat(
			res,
			ast_expr_getfuncs(expr->u.call.arg[i])
		);	
		/* XXX: leaks */
	}
	return res;
}

static struct string_arr *
ast_expr_range_getfuncs(struct ast_expr *expr)
{
	struct string_arr *res = string_arr_create();
	struct string_arr *lw = ast_expr_getfuncs(expr->u.range.lw),
			  *up = ast_expr_getfuncs(expr->u.range.up);
	string_arr_concat(res, lw);
	string_arr_concat(res, up);
	return res;
}

struct ast_declaration {
	char *name;
	struct ast_type *type;
};

static struct ast_declaration *
ast_declaration_create(char *name, struct ast_type *type)
{
	struct ast_declaration *decl = malloc(sizeof(struct ast_declaration));
	decl->name = name;
	decl->type = type;
	return decl;
}

char *
ast_declaration_name(struct ast_declaration *decl)
{
	return decl->name;
}

struct ast_type *
ast_declaration_type(struct ast_declaration *decl)
{
	return decl->type;
}

static struct ast_declaration *
unary_declare(struct ast_expr *, struct ast_type *base);

struct ast_declaration *
ast_expr_declare(struct ast_expr *expr, struct ast_type *base)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER: /* base */
		return ast_declaration_create(
			ast_expr_as_identifier(expr), base
		);
	case EXPR_UNARY:
		return unary_declare(expr, base);
	default:
		assert(false);
	}
}

static struct ast_declaration *
arr_declare(struct ast_expr *, struct ast_type *base);

static struct ast_declaration *
unary_declare(struct ast_expr *expr, struct ast_type *base)
{
	assert(ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE);

	struct ast_expr *operand = ast_expr_unary_operand(expr);
	switch (operand->kind) {
	case EXPR_BINARY: /* inner+size */
		return arr_declare(operand, base);
	default: /* inner */
		return ast_expr_declare(operand, ast_type_create_ptr(base));
	}
}

static struct ast_declaration *
arr_declare(struct ast_expr *expr, struct ast_type *base)
{
	assert(ast_expr_binary_op(expr) == BINARY_OP_ADDITION);

	struct ast_expr *e1 = ast_expr_binary_e1(expr),
			*e2 = ast_expr_binary_e2(expr);

	a_printf(
		ast_expr_isconstant(e2),
		"compound constant expressions not yet supported\n"
	);

	return ast_expr_declare(
		e1,
		ast_type_create_arr(
			base,
			ast_expr_as_constant(e2)
		)
	);

}

struct ast_expr *
ast_expr_declarator(struct ast_expr *expr)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
	case EXPR_UNARY:
		return expr;
	case EXPR_ASSIGNMENT:
		return ast_expr_assignment_lval(expr);
	default:
		assert(false);
	}
}

struct ast_expr *
ast_expr_initialiser(struct ast_expr *expr)
{
	switch (expr->kind) {
	case EXPR_IDENTIFIER:
	case EXPR_UNARY:
		return NULL;
	case EXPR_ASSIGNMENT:
		return ast_expr_assignment_rval(expr);
	default:
		assert(false);
	}
}

#include "verify.c"
