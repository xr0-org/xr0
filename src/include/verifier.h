#ifndef XR0_PATH
#define XR0_PATH

#include <stdbool.h>

struct ast_function;
struct externals;
struct state;
struct error;

struct verifier;

struct verifier *
verifier_create(struct ast_function *, struct externals *);

void
verifier_destroy(struct verifier *);

char *
verifier_str(struct verifier *);

bool
verifier_atend(struct verifier *);

typedef struct error *(progressor)(struct state *);

progressor *
progressor_step();

progressor *
progressor_next();

struct error *
verifier_progress(struct verifier *, progressor *);

struct error *
verifier_verify(struct verifier *, struct ast_expr *);

struct error *
verifier_setbreakpoint(struct verifier *);

struct lexememarker *
verifier_lexememarker(struct verifier *);

int
verifier_frameid(struct verifier *);

struct splitinstruct;
struct verifierinstruct;

struct verifierinstruct *
verifierinstruct_split(struct splitinstruct *);


#endif
