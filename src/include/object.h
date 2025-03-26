#ifndef OBJECT_H
#define OBJECT_H

struct value;
struct object;

DECLARE_RESULT_TYPE(struct object *, object, object_res)

struct object *
object_create(struct value *offset, struct value *value, struct value *size);

struct object *
object_copy(struct object *old);

struct state;

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

struct value *
object_offset(struct object *);

struct value *
object_size(struct object *);

int
object_contains(struct object *, struct value *offset, struct state *);

int
object_hasbefore(struct object *, struct value *offset, struct state *);

int
object_hasafter(struct object *, struct value *offset, struct state *);

/* object_upto: return the part of the object prior to offset.
 * asserts object_hasbefore(o, offset). */
struct object *
object_upto(struct object *o, struct value *offset, struct state *);

/* object_from: return the part of the object after the offset.
 * asserts object_hasafter(o, offset). */
struct object *
object_from(struct object *o, struct value *offset, struct state *);

/* object_at: return a size-1 object with the value of the object at the given
 * offset. */
struct object *
object_at(struct object *, struct value *offset);


struct state;
struct location;

int
object_references(struct object *, struct location *, struct state *,
		struct circuitbreaker *);

int
object_referencesheap(struct object *, struct state *, struct circuitbreaker *);

int
object_isdef(struct object *);

struct value *
object_as_defvalue(struct object *);

void
object_assign(struct object *, struct value *);

struct ast_type;

struct object *
object_getmember(struct object *obj, struct ast_type *t, char *member,
		struct state *s);

struct ast_type *
object_getmembertype(struct object *obj, struct ast_type *t, char *member,
		struct state *s);

#endif
