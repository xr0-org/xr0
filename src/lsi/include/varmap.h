#ifndef XR0_LSI_VARMAP_H

struct lsi_varmap;

struct lsi_varmap *
lsi_varmap_create(void);

void
lsi_varmap_destroy(struct lsi_varmap *);

char *
lsi_varmap_str(struct lsi_varmap *);

void
lsi_varmap_set(struct lsi_varmap *, char *k, char *v);

int
_lsi_varmap_haskey(struct lsi_varmap *, char *k);

char *
_lsi_varmap_get(struct lsi_varmap *, char *k);

#endif
