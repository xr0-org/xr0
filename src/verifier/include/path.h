#ifndef XR0_VERFIIER_PATH_H
#define XR0_VERFIIER_PATH_H

/* must be included after verifier.h */

struct state;
struct rconst;
struct ast_expr;
struct lexememarker;

struct path;

struct path *
path_create(struct state *);

struct path *
path_split(struct path *old, struct rconst *rconst, char *fname);

void
path_destroy(struct path *);

char *
path_str(struct path *);

int
path_atend(struct path *);

struct error *
path_progress(struct path *, progressor *prog);

struct error *
path_verify(struct path *, struct ast_expr *);

struct lexememarker *
path_lexememarker(struct path *);

struct error *
path_audit(struct path *abstract, struct path *actual);

#endif
