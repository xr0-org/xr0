#ifndef XR0_PATH
#define XR0_PATH

#include <stdbool.h>

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

bool
verifier_atend(struct verifier *);

typedef struct error *(progressor)(struct state *);

progressor *
progressor_step(void);

progressor *
progressor_next(void);

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

struct verifierinstruct;

struct splitinstruct;

struct verifierinstruct *
verifierinstruct_split(struct splitinstruct *);

struct splitinstruct;

struct splitinstruct *
splitinstruct_create(struct state *);

struct map;

void
splitinstruct_append(struct splitinstruct *, struct map *);

int 
splitinstruct_n(struct splitinstruct *);

struct map **
splitinstruct_splits(struct splitinstruct *);

struct state *
splitinstruct_state(struct splitinstruct *);

struct rconst;

struct rconst *
rconst_create(void);

struct rconst *
rconst_copy(struct rconst *);

void
rconst_destroy(struct rconst *);

char *
rconst_str(struct rconst *, char *indent);

DECLARE_RESULT_TYPE(char *, str, str_res)

struct str_res *
rconst_declare(struct rconst *, struct ast_expr *range, char *key, bool persist,
		struct state *);

struct str_res *
rconst_declarenokey(struct rconst *, struct ast_expr *range, bool persist,
		struct state *);

int
rconst_hasvar(struct rconst *, char *var);

char *
rconst_getidbykey(struct rconst *, char *key);

void
rconst_undeclare(struct rconst *);

bool
rconst_eval(struct rconst *, struct ast_expr *);

struct lsi_varmap;

struct error *
rconst_constraintverify(struct rconst *spec, struct rconst *impl,
		struct lsi_varmap *spec_impl_m);

#endif
