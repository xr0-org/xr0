#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "util.h"

struct error {
	enum error_type {
		ERROR_PRINTF,
		ERROR_VERIFIERINSTRUCT,
		ERROR_VERIFIERCONTRADICTION,
		ERROR_RETURN,
		ERROR_BREAK,
		ERROR_PREV,
		ERROR_CMD_VALIDATION,

		ERROR_BLOCK_OBSERVE_NOOBJ,

		ERROR_STATE_GET_NOBLOCK,
		ERROR_STATE_DEREF_RCONST,

		ERROR_VALUE_BOUNDS,

		ERROR_MODULATE_SKIP,

		ERROR_EVAL_VOID,

		ERROR_LSI_NOTFEASIBLE,
	} type;
	union error_contents {
		char *printf;
		struct verifierinstruct *inst;
	} contents;
	struct error *inner;
};

static struct error *
error_to(struct error *err, enum error_type t)
{
	assert(err);
	if (err->type == t) {
		return err;
	}
	if (err->inner) {
		return error_to(err->inner, t);
	}
	return NULL;
}

static char *
findnextfmt(char **start);

static struct error *
error_nest(struct error *outer, struct error *inner)
{
	if (!outer->inner) {
		outer->inner = inner;
		return outer;
	}
	return error_nest(outer->inner, inner);
}

struct error *
error_printf(char *fmt, ...)
{
	char *otherfmt; void *otherarg;
	struct error *inner;

	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_PRINTF;

	struct strbuilder *b = strbuilder_create();
	va_list ap;
	va_start(ap, fmt);
	for (char *p = fmt; *p; p++) {
		if (*p != '%') {
			strbuilder_putc(b, *p);
			continue;
		}
		/* ⊢ *p == '%' */
		switch (*++p) {
		case 'w':
			inner = va_arg(ap, struct error *);
			strbuilder_printf(
				b, "%s", error_str(inner)
			);
			err = error_nest(err, inner);
			break;
		default:
			otherfmt = findnextfmt(&p);
			otherarg = va_arg(ap, void *);
			strbuilder_printf(b, otherfmt, otherarg);
			free(otherfmt);
			p--; /* prepare for increment */
			break;
		}
	}
	char *s = strbuilder_build(b);
	va_end(ap);

	err->contents.printf = s;
	return err;
}

static char *
findnextfmt(char **p)
{
	char *start = *p;

	char *s = start;
	for (; *s && *s != '%'; s++) {}
	/* s is '\0' or '%' */
	int len = s - start;
	*p = s;
	char *output = malloc(sizeof(char) * (len + 2));
	*output = '%';
	strncpy(output + 1, start, len);
	*(output + len + 1) = '\0';
	return output;
}

struct error *
error_verifierinstruct(struct verifierinstruct *inst)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_VERIFIERINSTRUCT;
	err->contents.inst = inst;
	return err;
}

struct error *
error_to_verifierinstruct(struct error *err)
{
	return error_to(err, ERROR_VERIFIERINSTRUCT);
}

struct verifierinstruct *
error_get_verifierinstruct(struct error *err)
{
	assert(err->type == ERROR_VERIFIERINSTRUCT);
	return err->contents.inst;
}

struct error *
error_verifiercontradiction(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_VERIFIERCONTRADICTION;
	return err;
}

struct error *
error_to_verifiercontradiction(struct error *err)
{
	return error_to(err, ERROR_VERIFIERCONTRADICTION);
}

struct error *
error_return(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_RETURN;
	return err;
}

struct error *
error_to_return(struct error *err)
{
	return error_to(err, ERROR_RETURN);
}

struct error *
error_break(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_BREAK;
	return err;
}

struct error *
error_to_break(struct error *err)
{
	return error_to(err, ERROR_BREAK);
}

struct error *
error_prev(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_PREV;
	return err;
}

struct error *
error_to_prev(struct error *err)
{
	return error_to(err, ERROR_PREV);
}

struct error *
error_cmdvalidation(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_CMD_VALIDATION;
	return err;
}

struct error *
error_to_cmdvalidation(struct error *err)
{
	return error_to(err, ERROR_CMD_VALIDATION);
}

struct error *
error_block_observe_noobj(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_BLOCK_OBSERVE_NOOBJ;
	return err;
}

struct error *
error_to_block_observe_noobj(struct error *err)
{
	return error_to(err, ERROR_BLOCK_OBSERVE_NOOBJ);
}

struct error *
error_state_get_no_block(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_STATE_GET_NOBLOCK;
	return err;
}

struct error *
error_to_state_get_no_block(struct error *err)
{
	return error_to(err, ERROR_STATE_GET_NOBLOCK);
}

struct error *
error_state_deref_rconst(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_STATE_DEREF_RCONST;
	return err;
}

struct error *
error_to_state_deref_rconst(struct error *err)
{
	return error_to(err, ERROR_STATE_DEREF_RCONST);
}

struct error *
error_value_bounds(struct error *inner)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_VALUE_BOUNDS;
	err->inner = inner;
	return err;
}

struct error *
error_to_value_bounds(struct error *err)
{
	return error_to(err, ERROR_VALUE_BOUNDS);
}

struct error *
error_modulate_skip(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_MODULATE_SKIP;
	return err;
}

struct error *
error_to_modulate_skip(struct error *err)
{
	return error_to(err, ERROR_MODULATE_SKIP);
}

struct error *
error_eval_void(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_EVAL_VOID;
	return err;
}

struct error *
error_to_eval_void(struct error *err)
{
	return error_to(err, ERROR_EVAL_VOID);
}

struct error *
error_lsi_notfeasible(void)
{
	struct error *err = calloc(1, sizeof(struct error));
	err->type = ERROR_LSI_NOTFEASIBLE;
	return err;
}

struct error *
error_to_lsi_notfeasible(struct error *err)
{
	return error_to(err, ERROR_LSI_NOTFEASIBLE);
}

char *
error_str(struct error *err)
{
	char *error_type_str[] = {
		[ERROR_VERIFIERINSTRUCT]	= "verifier instruction",
		[ERROR_VERIFIERCONTRADICTION]	= "verifier contradiction",
		[ERROR_RETURN]			= "returned",
		[ERROR_BREAK]			= "broken",
		[ERROR_PREV]			= "prev",
		[ERROR_CMD_VALIDATION]		= "command validation",

		[ERROR_BLOCK_OBSERVE_NOOBJ]	= "block_observe no object",

		[ERROR_STATE_GET_NOBLOCK]	= "state_get no block",
		[ERROR_STATE_DEREF_RCONST]	= "state_deref rconst",

		[ERROR_VALUE_BOUNDS]		= "value bounds",

		[ERROR_MODULATE_SKIP]		= "modulate skip",

		[ERROR_EVAL_VOID]		= "eval void",

		[ERROR_LSI_NOTFEASIBLE]		= "not feasible",
	};

	switch (err->type) {
	case ERROR_PRINTF:
		return dynamic_str(err->contents.printf);
	case ERROR_VERIFIERINSTRUCT:
	case ERROR_VERIFIERCONTRADICTION:
	case ERROR_RETURN:
	case ERROR_BREAK:
	case ERROR_PREV:
	case ERROR_CMD_VALIDATION:
	case ERROR_BLOCK_OBSERVE_NOOBJ:
	case ERROR_STATE_GET_NOBLOCK:
	case ERROR_STATE_DEREF_RCONST:
	case ERROR_MODULATE_SKIP:
	case ERROR_EVAL_VOID:
	case ERROR_LSI_NOTFEASIBLE:
		return dynamic_str(error_type_str[err->type]);
	case ERROR_VALUE_BOUNDS:
		assert(err->inner);
		return error_str(err->inner);
	default:
		assert(false);
	}
}
