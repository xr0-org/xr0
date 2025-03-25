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

struct lsi_le;

struct splitinstruct *
splitinstruct_create(struct lsi_le *, struct lsi_le *);

struct lsi_le *
splitinstruct_0(struct splitinstruct *);

struct lsi_le *
splitinstruct_1(struct splitinstruct *);

struct rconst;

struct rconst *
rconst_create(void);

struct rconst *
rconst_split(struct rconst *, struct lsi_le *);

struct rconst *
rconst_copy(struct rconst *);

void
rconst_destroy(struct rconst *);

char *
rconst_str(struct rconst *, char *indent);

char *
rconst_declareorget(struct rconst *, char *key, bool persist, struct state *);

char *
rconst_declarenokey(struct rconst *, bool persist, struct state *);

struct error *
rconst_addconstraint(struct rconst *v, struct lsi_le *le);

DECLARE_RESULT_TYPE(char *, str, str_res)

struct str_res *
rconst_getwithconstvalue(struct rconst *, int);

struct lsi_le;

int
rconst_isanyint(struct rconst *, char *);

int
rconst_hasvar(struct rconst *, char *var);

void
rconst_undeclare(struct rconst *);

bool
rconst_eval(struct rconst *, struct ast_expr *);

struct lsi_varmap;

struct error *
rconst_constraintverify(struct rconst *spec, struct rconst *impl,
		struct lsi_varmap *spec_m, struct lsi_varmap *impl_m);

int
rconst_satisfies(struct rconst *, struct lsi_le *);

struct lsi_range;
struct lsi_expr;

struct lsi_range *
rconst_range_eval(struct rconst *, struct lsi_expr *);

int
rconst_isfeasible(struct rconst *, struct lsi_le *);

#endif
