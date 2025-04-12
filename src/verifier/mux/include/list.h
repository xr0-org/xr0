#ifndef XR0_VERIFIER_MUX_LIST
#define XR0_VERIFIER_MUX_LIST

struct list;

struct list *
list_create(void *destroy(void *));

void
list_destroy(struct list *);

void
list_add(struct list *, void *);

#endif
