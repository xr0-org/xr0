#ifndef XR0_BLOCK_NODE_H
#define XR0_BLOCK_NODE_H

/* o_node: an object node. */
struct o_node;

struct o_node *
o_node_create(struct object *);

struct o_node *
o_node_copy(struct o_node *);

void
o_node_destroy(struct o_node *);

char *
o_node_str(struct o_node *);

void
o_node_append(struct o_node *, struct object *);

struct state;

struct object *
o_node_observe(struct o_node *, int offset, struct state *);

struct ast_type;

struct lsi_varmap *
o_node_rconst_mapping(struct o_node *, struct ast_type *, struct state *,
		char *id);

struct error *
o_node_mutating_shapeverify(struct o_node *spec_n, struct o_node *impl_n,
		struct state *spec, struct state *impl, char *id,
		struct ast_type *);

#endif
