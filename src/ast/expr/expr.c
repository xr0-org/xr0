#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "math.h"
#include "util.h"
#include "expr.h"

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
	expr->u.constant = k;
	return expr;
}

int
ast_expr_as_constant(struct ast_expr *expr)
{
	assert(expr->kind == EXPR_CONSTANT);
	return expr->u.constant;
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
	return expr->root;
}

int
ast_expr_call_nargs(struct ast_expr *expr)
{
	return expr->u.call.n;
}

struct ast_expr **
ast_expr_call_args(struct ast_expr *expr)
{
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
ast_expr_member_deref_str_build(struct ast_expr *root, char *member,
		struct strbuilder *b);

static void
ast_expr_member_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	struct ast_expr *root = expr->root;

	if (root->kind == EXPR_UNARY) {
		return ast_expr_member_deref_str_build(root, expr->u.string, b);
	}

	char *r = ast_expr_str(root);
	strbuilder_printf(b, "%s.%s", r, expr->u.string);
	free(root);
}

static void
ast_expr_member_deref_str_build(struct ast_expr *root, char *member,
		struct strbuilder *b)
{
	assert(ast_expr_unary_op(root) == UNARY_OP_DEREFERENCE);

	struct ast_expr *inner = ast_expr_unary_operand(root); /* e1+e2 */
	struct ast_expr *e1 = ast_expr_binary_e1(inner),
			*e2 = ast_expr_binary_e2(inner);

	char *left = ast_expr_str(e1);
	if (e2->kind == EXPR_CONSTANT && ast_expr_as_constant(e2) == 0) { 
		strbuilder_printf(b, "%s->%s", left, member);
	} else {
		char *index = ast_expr_str(e2);
		strbuilder_printf(b, "%s[%s].%s", left, index, member);
		free(index);
	}
	free(left);

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
ast_expr_unary_isdereference(struct ast_expr *expr)
{
	return ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE;
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

static void
ast_expr_binary_str_build(struct ast_expr *expr, struct strbuilder *b)
{
	const char *opstr[] = {
		[BINARY_OP_EQ]		= "==",
		[BINARY_OP_NE]		= "!=",

		[BINARY_OP_LT]		= "<",
		[BINARY_OP_GT]		= ">",
		[BINARY_OP_LE]		= "<=",
		[BINARY_OP_GE]		= ">=",

		[BINARY_OP_ADDITION]	= "+",
		[BINARY_OP_SUBTRACTION]	= "-",
	};
	char *e1 = ast_expr_str(expr->u.binary.e1),
	     *e2 = ast_expr_str(expr->u.binary.e2);
	strbuilder_printf(b, "%s%s%s", e1, opstr[expr->u.binary.op], e2);
	free(e1);
	free(e2);
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
ast_expr_arbarg_create()
{
	struct ast_expr *expr = ast_expr_create();
	expr->kind = EXPR_ARBARG;
	return expr;
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
	case EXPR_ARBARG:
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
		strbuilder_printf(b, "%d", expr->u.constant);
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
	case EXPR_ARBARG:
		strbuilder_putc(b, '$');
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
		return ast_expr_constant_create(expr->u.constant);
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
	case EXPR_ARBARG:
		return ast_expr_arbarg_create();
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
		return e1->u.constant == e2->u.constant;
	case EXPR_IDENTIFIER:
		return strcmp(ast_expr_as_identifier(e1), ast_expr_as_identifier(e2)) == 0;
	case EXPR_ASSIGNMENT:
		return ast_expr_equal(e1->root, e2->root)
			&& ast_expr_equal(e1->u.assignment_value, e2->u.assignment_value); 
	case EXPR_BINARY:
		return ast_expr_binary_op(e1) == ast_expr_binary_op(e2) &&
			ast_expr_equal(ast_expr_binary_e1(e1), ast_expr_binary_e1(e2)) && 
			ast_expr_equal(ast_expr_binary_e2(e1), ast_expr_binary_e2(e2));
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
		if (e->u.constant < 0) {
			return math_expr_neg_create(
				math_expr_atom_create(
					math_atom_nat_create(-e->u.constant)
				)
			);
		}
		return math_expr_atom_create(
			math_atom_nat_create(e->u.constant)
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

#include "verify.c"
