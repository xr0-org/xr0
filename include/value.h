#ifndef VALUE_H
#define VALUE_H

enum value_type {
	VALUE_SYNC,
	VALUE_PTR,
	VALUE_INT,
	VALUE_LITERAL,
	VALUE_STRUCT,
};

struct value;

struct location;

struct value *
value_ptr_create(struct location *loc);

struct value *
value_ptr_indefinite_create();

struct value *
value_int_create(int val);

struct value *
value_literal_create(char *);

struct value *
value_int_indefinite_create();

struct value *
value_int_ne_create(int not_val);

struct value *
value_int_range_create(int lw, int excl_up);

int
value_int_lw(struct value *);

int
value_int_up(struct value *);

struct value *
value_sync_create(struct ast_expr *);

struct value *
value_struct_create(struct ast_type *);

bool
value_isstruct(struct value *v);

struct value *
value_struct_indefinite_create(struct ast_type *, struct state *,
		char *comment, bool persist);

struct value *
value_pf_augment(struct value *, struct ast_expr *root);

struct ast_type *
value_struct_membertype(struct value *, char *member);

struct object *
value_struct_member(struct value *, char *member);

struct value *
value_copy(struct value *);

struct value *
value_abstractcopy(struct value *, struct state *s);

void
value_destroy(struct value *);

char *
value_str(struct value *);

enum value_type
value_type(struct value *);

bool
value_islocation(struct value *);

struct location *
value_as_location(struct value *);

bool
value_referencesheap(struct value *, struct state *);

bool
value_isconstant(struct value *v);

int
value_as_constant(struct value *v);

bool
value_issync(struct value *v);

struct ast_expr *
value_as_sync(struct value *v);

struct ast_expr *
value_to_expr(struct value *);

bool
value_isliteral(struct value *v);

char *
value_as_literal(struct value *v);

bool
value_references(struct value *, struct location *, struct state *);

enum ast_binary_operator;

bool
value_equal(struct value *v1, struct value *v2);

/* value_assume: Returns false if contradiction encountered. */
bool
value_assume(struct value *, bool value);

enum number_value_type {
	NUMBER_VALUE_CONSTANT,
	NUMBER_VALUE_LIMIT,
};

struct number_value;

struct number_value *
number_value_create(enum number_value_type, int contant, bool max);

void
number_value_destroy(struct number_value *);

struct number_range;

struct number_range *
number_range_create(struct number_value *lw, struct number_value *up);

void
number_range_destroy(struct number_range *);

struct number_range_arr;

struct number_range_arr *
number_range_arr_create();

void
number_range_arr_destroy(struct number_range_arr *arr);

int
number_range_arr_n(struct number_range_arr *);

struct number_range **
number_range_arr_range(struct number_range_arr *);

int
number_range_arr_append(struct number_range_arr *, struct number_range *);


enum number_type {
	NUMBER_RANGES,
	NUMBER_COMPUTED,
};

struct number;

struct number *
number_single_create(int val);

void
number_destroy(struct number *);

char *
number_str(struct number *);

#endif
