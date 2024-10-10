#ifndef XR0_STATE_CONSTRAINT
#define XR0_STATE_CONSTRAINT

struct constraint;

struct constraint *
constraint_create(struct ast_type *, struct state *spec, struct state *impl);

struct error;

struct error *
constraint_verify(struct constraint *, struct value *spec_v, struct value *impl_v);

struct error *
ast_specval_verify(struct ast_type *t, struct value *param, struct value *arg,
		struct state *spec, struct state *caller);

#endif
