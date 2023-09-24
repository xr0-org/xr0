#ifndef VALUE_H
#define VALUE_H

enum value_type {
	VALUE_PTR,
	VALUE_VIRTUAL,
};

struct value;

struct location;

struct value *
value_ptr_create(struct location *loc);

struct value *
value_virt_create(struct location *virt);

struct value *
value_copy(struct value *);

void
value_destroy(struct value *);

char *
value_str(struct value *);

enum value_type
value_type(struct value *);

struct location *
value_location(struct value *);

struct location *
value_as_ptr(struct value *);

struct location *
value_as_virt(struct value *);

bool
value_heap_equivalent(struct value *v1, struct value *v2, struct stack *s1, 
		struct stack *s2, struct heap *h1, struct heap *h2);

#endif
