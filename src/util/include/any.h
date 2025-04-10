#ifndef XR0_UTIL_ANY_H
#define XR0_UTIL_ANY_H

/* struct any: a container capable of storing any value. */
struct any;

struct any *
any_int(int);

struct any *
any_ptr(void *);

/* any_destroy: destroy the container. the caller is responsible to destroy any
 * objects referenced by a ptr case. */
void
any_destroy(struct any *);

int
any_as_int(struct any *);

void *
any_as_ptr(struct any *);

#endif
