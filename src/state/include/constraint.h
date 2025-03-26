#ifndef XR0_STATE_CONSTRAINT
#define XR0_STATE_CONSTRAINT

struct constraint;

struct constraint *
constraint_create(char *id, struct ast_type *t, struct state *spec,
		struct state *impl);

void
constraint_destroy(struct constraint *);

struct error;

struct error *
constraint_shapeverify(struct constraint *, struct object *spec_obj,
		struct object *impl_obj);

#endif
