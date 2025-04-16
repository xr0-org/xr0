#ifndef XR0_TEXT_H
#define XR0_TEXT_H

struct text;

struct ast_block;

struct text *
text_create(struct ast_block *);

void
text_destroy(struct text *);


struct text_pc;

/* text_pc_create: return a pc pointing at the 0th statement of a text. */
struct text_pc *
text_pc_create(void);

struct text_pc *
text_pc_copy(struct text_pc *);

void
text_pc_destroy(struct text_pc *);

char *
text_pc_rendertop(struct text_pc *, struct text *);

void
text_pc_advance(struct text_pc *, struct text *);

/* text_pc_enter: enter the nested block in the text that the pc is currently
 * pointing at. */
void
text_pc_enter(struct text_pc *, struct text *);

/* text_pc_enterlinear: enter the generated block. */
void
text_pc_enterlinear(struct text_pc *, struct text *, struct ast_block *gen);

struct ast_stmt;

struct ast_stmt *
text_pc_getstmt(struct text_pc *, struct text *);

struct ast_stmt *
text_pc_prevstmt(struct text_pc *, struct text *);

int
text_pc_atblockend(struct text_pc *, struct text *);

#endif
