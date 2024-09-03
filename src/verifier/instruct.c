#include <stdlib.h>
#include <assert.h>

#include "verifier.h"

#include "intern.h"
#include "instruct.h"

struct verifierinstruct {
	enum verifierinstruct_type {
		PATHINSTRUCT_SPLIT,
	} type;
	union {
		struct splitinstruct *split;
	};
};

struct verifierinstruct *
verifierinstruct_split(struct splitinstruct *s)
{
	struct verifierinstruct *inst = malloc(sizeof(struct verifierinstruct));
	assert(inst);
	inst->type = PATHINSTRUCT_SPLIT;
	inst->split = s;
	return inst;
}

void
verifierinstruct_do(struct verifierinstruct *inst, struct verifier *p)
{
	switch (inst->type) {
	case PATHINSTRUCT_SPLIT:
		verifier_split(p, inst->split);
		break;
	default:
		assert(false);
	}
}
