#ifndef XR0_STATE_CONSTRAINT
#define XR0_STATE_CONSTRAINT

struct constraint;

struct constraint *
constraint_create(struct state *spec, struct state *impl, struct ast_type *);

struct constraint *
constraint_deref(struct constraint *);

void
constraint_destroy(struct constraint *);

struct error;

struct error *
constraint_shapeverify(struct constraint *, struct value *spec_v,
		struct value *impl_v);

struct error *
constraint_shapeverify_object(struct constraint *, struct object *spec_obj,
		struct location *impl_loc);

#endif
