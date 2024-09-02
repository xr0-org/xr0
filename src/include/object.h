#ifndef OBJECT_H
#define OBJECT_H

struct value;
struct object;

struct ast_expr;

struct object *
object_value_create(struct ast_expr *offset, struct value *);

struct object *
object_copy(struct object *old);

struct object *
object_abstractcopy(struct object *old, struct state *s);

struct int_arr;

struct circuitbreaker;

struct state;

struct int_arr *
object_deriveorder(struct object *, struct circuitbreaker *, struct state *);

struct permutation;

struct object *
object_permuteheaplocs(struct object *, struct permutation *);

void
object_destroy(struct object *);

char *
object_str(struct object *);

struct ast_expr *
object_lower(struct object *);

struct ast_expr *
object_upper(struct object *);

struct state;

bool
object_isdeallocand(struct object *, struct state *);

struct location;

bool
object_references(struct object *, struct location *, struct state *,
		struct circuitbreaker *);


bool
object_referencesheap(struct object *, struct state *, struct circuitbreaker *);

bool
object_hasvalue(struct object *);

struct value *
object_as_value(struct object *);

struct error *
object_assign(struct object *, struct value *);

struct error *
object_transfigure(struct object *obj, struct value *val, struct state *actual,
		struct state *compare, bool islval);

bool
object_contains(struct object *, struct ast_expr *, struct state *);

bool
object_contig_precedes(struct object *before, struct object *after,
		struct state *);

struct object *
object_upto(struct object *, struct ast_expr *excl_upper, struct state *);

struct object *
object_from(struct object *, struct ast_expr *incl_lower, struct state *);

struct object *
object_getmember(struct object *obj, struct ast_type *t, char *member,
		struct state *s);

struct ast_type *
object_getmembertype(struct object *obj, struct ast_type *t, char *member,
		struct state *s);

struct error *
object_dealloc(struct object *, struct state *);


struct heap;


struct object_arr;

struct object_arr *
object_arr_create();

void
object_arr_destroy(struct object_arr *);

struct object_arr *
object_arr_copy(struct object_arr *);

int
object_arr_nobjects(struct object_arr *);

struct object **
object_arr_objects(struct object_arr *);

int
object_arr_index(struct object_arr *arr, struct ast_expr *offset, struct state *);

int
object_arr_index_upperincl(struct object_arr *arr, struct ast_expr *offset,
		struct state *);

int
object_arr_insert(struct object_arr *arr, int index, struct object *);

int
object_arr_append(struct object_arr *arr, struct object *obj);

void
object_arr_remove(struct object_arr *arr, int index);

DECLARE_RESULT_TYPE(struct object *, object, object_res)

#endif
