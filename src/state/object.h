#ifndef OBJECT_H
#define OBJECT_H

struct value;

struct object;

struct object *
object_create(struct ast_expr *lower, struct ast_expr *upper);

void
object_destroy(struct object *);

char *
object_str(struct object *obj);

struct ast_expr *
object_lower(struct object *obj);

struct ast_expr *
object_upper(struct object *obj);

struct value *
object_value(struct object *);

void
object_assign(struct object *, struct value *);

bool
object_contains(struct object *, struct ast_expr *);

bool
object_isempty(struct object *);

bool
object_isvirtual(struct object *);

bool
object_contig_precedes(struct object *before, struct object *after);

struct object *
object_upto(struct object *, struct ast_expr *excl_upper, struct heap *);

struct object *
object_from(struct object *, struct ast_expr *incl_lower, struct heap *);


struct heap;


struct object_arr;

struct object_arr *
object_arr_create();

void
object_arr_destroy(struct object_arr *);

int
object_arr_nobjects(struct object_arr *);

struct object **
object_arr_objects(struct object_arr *);

int
object_arr_index(struct object_arr *arr, struct ast_expr *offset);

int
object_arr_index_upperincl(struct object_arr *arr, struct ast_expr *offset);

void
object_arr_insert(struct object_arr *arr, int index, struct object *);

void
object_arr_append(struct object_arr *arr, struct object *obj);

void
object_arr_delete(struct object_arr *arr, int index);

#endif
