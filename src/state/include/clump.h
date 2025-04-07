#ifndef CLUMP_H
#define CLUMP_H

#include "block.h"

struct clump;

struct clump *
clump_create(void);

void
clump_destroy(struct clump *);

char *
clump_str(struct clump *, char * indent);

struct clump *
clump_copy(struct clump *);

int
clump_newblock(struct clump *, int size);

struct block;

struct block *
clump_getblock(struct clump *c, int address);

struct rconst;

bool
clump_callerreferences(struct clump *, struct location *, struct state *,
		struct rconst *);

void
clump_undeclare(struct clump *, struct state *, struct rconst *);

#endif
