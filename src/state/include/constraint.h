#ifndef XR0_STATE_CONSTRAINT
#define XR0_STATE_CONSTRAINT

struct constraint;

struct constraint *
constraint_create(struct state *spec, struct state *impl, struct ast_type *);

void
constraint_destroy(struct constraint *);

struct error;

struct error *
constraint_verify(struct constraint *, struct value *spec_v, struct value *impl_v);

#endif
