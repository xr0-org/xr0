#ifndef XR0_TEXT_H
#define XR0_TEXT_H

struct text;

struct ast_block;

struct text *
text_create(struct ast_block *);

void
text_destroy(struct text *);


struct text_pc;

struct text_pc *
text_pc_create(int);

void
text_pc_destroy(struct text_pc *);

void
text_pc_add(struct text_pc *, int);

struct ast_stmt *
text_pc_getstmt(struct text_pc *, struct text *);

int
text_pc_atblockend(struct text_pc *, struct text *);

#endif
