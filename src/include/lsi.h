#ifndef XR0_LSI_H
#define XR0_LSI_H

/* lsi: a system of linear inequalities */
struct lsi;

struct lsi *
lsi_create(void);

struct lsi *
lsi_copy(struct lsi *);

void
lsi_destroy(struct lsi *);

char *
lsi_str(struct lsi *, char *prefix);

struct error;
struct lsi_le;

struct error *
lsi_add(struct lsi *, struct lsi_le *);

struct error *
lsi_addrange(struct lsi *, struct lsi *);

struct lsi_varmap;

/* lsi_userspace_project: given an lsi and a mapping of names into variables
 * within the lsi, create a system where every unique name is constrained the
 * way its corresponding variable is in the original system. */
struct lsi *
lsi_userspace_project(struct lsi *, struct lsi_varmap *name_var_m);

struct lsi *
lsi_prefixvars(struct lsi *, char *prefix);

struct string_arr;

/* lsi_eliminate_except: use Fourier-Motzkin elimination to produce an
 * equivalent system with no variables that don't appear in the given array. */
struct lsi *
lsi_eliminate_except(struct lsi *, struct string_arr *);

int
lsi_var_isconst(struct lsi *, char *var, int c);

int
lsi_var_isanyint(struct lsi *, char *);

/* lsi_checksatisfiesrange: return an error if there is a inequality in m which
 * isn't satisfied in l. */
struct error *
lsi_checksatisfiesrange(struct lsi *l, struct lsi *m);

/* lsi_satisfies: similar to the above except for a single inequality */
int
lsi_satisfies(struct lsi *, struct lsi_le *);

struct lsi_range;
struct lsi_expr;

/* lsi_range_eval: returns a range of the inclusive lower and upper bounds of the
 * expression within the system. */
struct lsi_range *
lsi_range_eval(struct lsi *, struct lsi_expr *);

/* lsi_le: a less-than-or-equal-to inequality */
struct lsi_le;

struct lsi_expr;

/* le_create: l <= r */
struct lsi_le *
lsi_le_create(struct lsi_expr *l, struct lsi_expr *r);

struct lsi_le *
lsi_le_negate(struct lsi_le *);

struct lsi_le *
lsi_le_copy(struct lsi_le *);

void
lsi_le_destroy(struct lsi_le *);

char *
lsi_le_str(struct lsi_le *);


/* lsi_expr: an arithmetic term involving constants, variables, sums and
 * products */ 
struct lsi_expr;

struct lsi_expr *
lsi_expr_const_create(int);

struct lsi_expr *
lsi_expr_var_create(char *);

struct lsi_expr *
lsi_expr_sum_create(struct lsi_expr *, struct lsi_expr *);

struct lsi_expr *
lsi_expr_product_create(struct lsi_expr *, struct lsi_expr *);

struct lsi_expr *
lsi_expr_copy(struct lsi_expr *);

void
lsi_expr_destroy(struct lsi_expr *);

char *
lsi_expr_str(struct lsi_expr *);


struct lsi_range;

struct lsi_range *
lsi_range_create(int lw, int up);

void
lsi_range_destroy(struct lsi_range *);

char *
lsi_range_str(struct lsi_range *);

int
lsi_range_isconst(struct lsi_range *);

int
lsi_range_as_const(struct lsi_range *);

/* lsi_range_expr_le_lw: return an inequality expressing e <= lw where lw is
 * the lower bound of the range */
struct lsi_le *
lsi_range_expr_le_lw(struct lsi_range *, struct lsi_expr *e);


/* lsi_varmap: a name->name mapping of variables */
struct lsi_varmap;

struct lsi_varmap *
lsi_varmap_create(void);

struct lsi_varmap *
lsi_varmap_prefix(struct lsi_varmap *, char *key, char *val);

struct lsi_varmap *
lsi_varmap_copy(struct lsi_varmap *);

void
lsi_varmap_destroy(struct lsi_varmap *);

char *
lsi_varmap_str(struct lsi_varmap *);

void
lsi_varmap_set(struct lsi_varmap *, char *k, char *v);

void
lsi_varmap_setvaluealias(struct lsi_varmap *, char *k, char *alias);

struct lsi_varmap *
lsi_varmap_valuealias_map(struct lsi_varmap *);

/* lsi_varmap_addrange: copy m0's key-value pairs to m, asserting that any common
 * keys must have the same value in both maps. */
void
lsi_varmap_addrange(struct lsi_varmap *m, struct lsi_varmap *m0);

struct string_arr;

struct string_arr *
lsi_varmap_values(struct lsi_varmap *);

/* lsi_var_names_map: given a (not-necessarily injective) mapping of names onto
 * vars, return a unique mapping of vars onto string arrays of names. */
struct map *
lsi_varmap_var_names_map(struct lsi_varmap *name_var_m);

#endif
