#ifndef STATIC_H
#define STATIC_H

#include "block.h"

struct static_memory;

struct static_memory *
static_memory_create();

void
static_memory_destroy(struct static_memory *);

char *
static_memory_str(struct static_memory *, char * indent);

struct static_memory *
static_memory_copy(struct static_memory *);

struct value;

int
static_memory_newblock(struct static_memory *);

struct block;

struct block *
static_memory_getblock(struct static_memory *, int address);

void
static_memory_stringpool(struct static_memory *sm, char *lit, struct location *);

struct location *
static_memory_checkpool(struct static_memory *, char *);

#endif
