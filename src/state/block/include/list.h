#ifndef XR0_BLOCK_LIST_H
#define XR0_BLOCK_LIST_H

struct o_list;

struct o_list *
o_list_create(void);

struct o_list *
o_list_copy(struct o_list *);

void
o_list_destroy(struct o_list *);

char *
o_list_str(struct o_list *);

void
o_list_add(struct o_list *, struct object *);

struct state;

struct object *
o_list_observe(struct o_list *, int offset, struct state *); 

int
o_list_references(struct o_list *, struct location *, struct state *,
		struct circuitbreaker *);

struct error *
o_list_mutating_shapeverify(struct o_list *spec_l, struct o_list *impl_l,
		struct state *spec, struct state *impl, char *id,
		struct ast_type *);

struct lsi_varmap;

struct lsi_varmap *
o_list_rconst_mapping(struct o_list *, struct ast_type *,
		struct state *, char *id);


#endif
