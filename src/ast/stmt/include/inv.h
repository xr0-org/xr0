#ifndef XR0_AST_STMT_INV_H
#define XR0_AST_STMT_INV_H

struct inv;

struct inv *
inv_create(struct ast_block *);

struct inv *
inv_copy(struct inv *);

void
inv_destroy(struct inv *);

char *
inv_str(struct inv *, int indent_level);

void
inv_addlabel(struct inv *, char *);

struct error;

struct error *
inv_exec(struct inv *, struct state *);

#endif
