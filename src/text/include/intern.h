#ifndef XR0_TEXT_TEXT_H
#define XR0_TEXT_TEXT_H

struct text *
text_getnext(struct text *, int);

struct ast_block;

void
text_putgen(struct text *, int, struct ast_block *gen);

struct ast_stmt *
text_getstmt(struct text *, int);

int
text_has(struct text *, int);

int
text_atend(struct text *, int);

#endif
