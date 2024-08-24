#ifndef XR0_PATH
#define XR0_PATH

#include <stdbool.h>

struct ast_function;
struct externals;
struct state;
struct error;

struct path;

struct path *
path_create(struct ast_function *, struct externals *);

void
path_destroy(struct path *);

char *
path_str(struct path *);

bool
path_atend(struct path *);

typedef struct error *(progressor)(struct state *);

progressor *
progressor_step();

progressor *
progressor_next();

struct error *
path_progress(struct path *, progressor *);

struct error *
path_verify(struct path *, struct ast_expr *);

struct error *
path_setbreakpoint(struct path *);

struct lexememarker *
path_lexememarker(struct path *);

int
path_frameid(struct path *);

struct splitinstruct;
struct pathinstruct;

struct pathinstruct *
pathinstruct_split(struct splitinstruct *);

struct ast_expr;

struct pathinstruct *
pathinstruct_call(struct ast_expr *);

#endif
