#ifndef XR0_STATE_CONSTRAINT
#define XR0_STATE_CONSTRAINT

struct constraint;

struct constraint *
constraint_create(struct state *spec, struct state *impl, struct ast_type *);

void
constraint_destroy(struct constraint *);

struct error;

struct error *
constraint_shapeverify(struct constraint *, struct value *spec_v,
		struct value *impl_v);

struct error *
constraint_shapeverify_object(struct constraint *c, struct object *spec_obj,
		struct location *impl_loc);

struct lsi_varmap *
constraint_rconstmapping(struct constraint *c, struct value *spec_v,
		struct value *impl_v);

struct lsi_varmap *
constraint_rconstmapping_object(struct constraint *c,
		struct object *spec_obj, struct location *impl_loc);
#endif
