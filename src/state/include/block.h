#ifndef BLOCK_H
#define BLOCK_H

struct block;

struct block *
block_create(struct value *size);

struct block *
block_callercreate(struct value *size);

void
block_destroy(struct block *);

char *
block_str(struct block *);

struct error;
struct value;
struct ast_expr;

struct stack;
struct heap;
struct object;

struct state;
struct object_res;

struct object_res *
block_observe(struct block *, struct ast_expr *offset, struct state *);

struct location;

struct circuitbreaker;

bool
block_references(struct block *, struct location *, struct state *,
		struct circuitbreaker *);

bool
block_iscaller(struct block *);

struct state;

void
block_undeclare(struct block *, struct state *);

struct permutation;

struct block *
block_permuteheaplocs(struct block *, struct permutation *);

struct error *
block_shapeverify(struct block *spec_b, struct block *impl_b, struct state *spec,
		struct state *impl, char *block_name,
		struct ast_type *elem_type);

struct lsi_varmap;

struct lsi_varmap *
block_rconst_mapping(struct block *, struct ast_type *t, struct state *,
		char *referent);

struct block_arr;

struct block_arr *
block_arr_create(void);

void
block_arr_destroy(struct block_arr *);

struct block_arr *
block_arr_copy(struct block_arr *);

struct block **
block_arr_blocks(struct block_arr *);

int
block_arr_nblocks(struct block_arr *);

/* block_arr_append: Append struct block to array and return index (address). */
int
block_arr_append(struct block_arr *, struct block *);

void
block_arr_delete(struct block_arr *, int address);

DECLARE_RESULT_TYPE(struct block *, block, block_res)

struct permutation;

struct permutation *
permutation_create(struct int_arr *);

struct permutation *
permutation_inverse_create(struct int_arr *);

struct permutation *
permutation_copy(struct permutation *);

void
permutation_destroy(struct permutation *);

char *
permutation_str(struct permutation *);

int
permutation_apply(struct permutation *, int);

int
permutation_applyinverse(struct permutation *, int);

#endif
