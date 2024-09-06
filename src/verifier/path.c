#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lex.h"
#include "state.h"
#include "util.h"
#include "verifier.h"

#include "path.h"
#include "segment.h"

struct path {
	enum path_phase {
		PATH_PHASE_ABSTRACT,
		PATH_PHASE_ACTUAL,
		PATH_PHASE_AUDIT,
		PATH_PHASE_ATEND,
	} phase;
	struct segment *abstract, *actual;
};

struct path *
path_create(struct state *abstract, struct state *actual)
{
	struct path *p = malloc(sizeof(struct path));
	assert(p);
	p->phase = PATH_PHASE_ABSTRACT;
	p->abstract = segment_create_withstate(abstract);
	p->actual = segment_create_withstate(actual);
	return p;
}

struct path *
path_copywithsplit(struct path *old, struct rconst *rconst, char *fname)
{
	struct path *p = malloc(sizeof(struct path));
	assert(p);
	p->phase = old->phase;
	switch (old->phase) {
	case PATH_PHASE_ABSTRACT:
	case PATH_PHASE_ACTUAL:
		p->abstract = segment_copywithsplit(old->abstract, rconst, fname);
		p->actual = segment_copywithsplit(old->actual, rconst, fname);
		break;
	default:
		assert(false);
	}
	return p;
}

void
path_destroy(struct path *p)
{
	/*state_destroy(p->abstract);*/
	/*state_destroy(p->actual);*/
	free(p);
}

char *
path_str(struct path *p)
{
	switch (p->phase) {
	case PATH_PHASE_ABSTRACT:
		return segment_str(p->abstract, "ABSTRACT");
	case PATH_PHASE_ACTUAL:
		return segment_str(p->actual, "ACTUAL");
	case PATH_PHASE_AUDIT:
		return dynamic_str("phase:\tAUDIT\n");
		break;
	case PATH_PHASE_ATEND:
		return dynamic_str("phase:\tEND\n");
	default:
		assert(false);
	}
}

int
path_atend(struct path *p)
{
	return p->phase == PATH_PHASE_ATEND;
}

struct error *
path_progress(struct path *p, progressor *prog)
{
	switch (p->phase) {
	case PATH_PHASE_ABSTRACT:
		if (segment_atend(p->abstract)) {
			p->phase = PATH_PHASE_ACTUAL;
			return path_progress(p, prog);
		}
		return segment_progress(p->abstract, prog);
	case PATH_PHASE_ACTUAL:
		if (segment_atend(p->actual)) {
			p->phase = PATH_PHASE_AUDIT;
			return path_progress(p, prog);
		}
		return segment_progress(p->actual, prog);
	case PATH_PHASE_AUDIT:
		p->phase = PATH_PHASE_ATEND;
		return segment_audit(p->abstract, p->actual);
	case PATH_PHASE_ATEND:
	default:
		assert(false);
	}
}

struct error *
path_verify(struct path *p, struct ast_expr *expr)
{
	switch (p->phase) {
	case PATH_PHASE_ABSTRACT:
		return segment_verify(p->abstract, expr);
	case PATH_PHASE_ACTUAL:
		return segment_verify(p->actual, expr);
	case PATH_PHASE_AUDIT:
	case PATH_PHASE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}

struct lexememarker *
path_lexememarker(struct path *p)
{
	switch (p->phase) {
	case PATH_PHASE_ABSTRACT:
		return segment_lexememarker(p->abstract);
	case PATH_PHASE_ACTUAL:
		return segment_lexememarker(p->actual);
	case PATH_PHASE_AUDIT:
	case PATH_PHASE_ATEND:
		return NULL;
	default:
		assert(false);
	}
}
