#ifndef XR0_VERFIIER_SEGMENT
#define XR0_VERFIIER_SEGMENT

/* must be included after verifier.h */

struct state;
struct rconst;
struct ast_expr;
struct lexememarker;

struct segment;

struct segment *
segment_create_withstate(struct state *);

struct segment *
segment_split(struct segment *old, struct rconst *rconst, char *fname);

void
segment_destroy(struct segment *);

char *
segment_str(struct segment *, char *phase);

int
segment_atend(struct segment *);

struct error *
segment_progress(struct segment *, progressor *prog);

struct error *
segment_verify(struct segment *, struct ast_expr *);

struct lexememarker *
segment_lexememarker(struct segment *);

struct error *
segment_audit(struct segment *abstract, struct segment *actual);

#endif
