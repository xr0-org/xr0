#ifndef XR0_VERIFIER_PATH
#define XR0_VERIFIER_PATH

/* must be included after verifier.h */

struct rconst;
struct ast_function;
struct externals;

struct path *
path_create(struct state *abstract, struct state *actual);

void
path_destroy(struct path *);

char *
path_str(struct path *);

int
path_atend(struct path *);

struct error *
path_progress(struct path *, progressor *);

struct path *
path_split(struct path *, struct rconst *, char *fname);

struct error *
path_verify(struct path *, struct ast_expr *);

struct lexememarker *
path_lexememarker(struct path *);

#endif
