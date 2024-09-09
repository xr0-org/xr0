#ifndef XR0_PATH
#define XR0_PATH

struct ast_function;
struct ast_expr;
struct externals;
struct error;
struct state;
struct value;

struct verifier;

struct verifier *
verifier_create(struct ast_function *, struct externals *);

void
verifier_destroy(struct verifier *);

char *
verifier_str(struct verifier *);

int
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

struct rconst;

struct rconst *
rconst_create();

struct rconst *
rconst_copy(struct rconst *);

void
rconst_destroy(struct rconst *);

char *
rconst_str(struct rconst *, char *indent);

char *
rconst_declare(struct rconst *, struct value *, char *key, int ispersist);

char *
rconst_declarenokey(struct rconst *, struct value *, int ispersist);

struct value *
rconst_get(struct rconst *, char *id);

char *
rconst_getidbykey(struct rconst *, char *key);

void
rconst_undeclare(struct rconst *);

int
rconst_eval(struct rconst *, struct ast_expr *);

#endif
