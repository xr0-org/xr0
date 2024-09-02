#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "ast.h"
#include "ext.h"
#include "util.h"
#include "stmt.h"
#include "function.h"

/*
 * map<node, string_arr> g maps node to adjacent neighbouring nodes
 */

static struct map *
calculate_indegrees(struct map *g);

static struct string_arr *
build_indegree_zero(struct map *indegrees);

struct string_arr *
topological_order(char *fname, struct externals *ext)
{
	struct string_arr *order = string_arr_create();

	struct map *g = ast_function_buildgraph(fname, ext); 
	struct map *indegrees = calculate_indegrees(g);
	struct string_arr *indegree_zero = build_indegree_zero(indegrees);
	/* while there are nodes of indegree zero */
	while (string_arr_n(indegree_zero) > 0) {
		/* add one node with indegree zero to ordered */
		char *curr = string_arr_deque(indegree_zero);
		string_arr_append(order, curr);

		for (int i = 0; i < g->n; i++) {
			struct entry e = g->entry[i];
			struct string_arr *v = (struct string_arr *) map_get(g, e.key);
			if (string_arr_contains(v, curr)) {
				/* decrement indegree */
				int *count = (int *) map_get(indegrees, e.key);
				*count = *count - 1;
				if (*count == 0) {
					string_arr_append(indegree_zero, dynamic_str(e.key));
				}
			}
		}
	}

	/* no more nodes with incoming edges */
	if (string_arr_n(order) != indegrees->n) {
		/* TODO: pass up error */
		fprintf(stderr, "cycle detected in graph\n");
		exit(EXIT_FAILURE);
	}

	return order;
}

static int *
dynamic_int(int i);

static struct map *
calculate_indegrees(struct map *g)
{
	struct map *indegrees = map_create();
	for (int i = 0; i < g->n; i++) {
		struct entry e = g->entry[i];
		struct string_arr *deps = (struct string_arr *) map_get(g, e.key);

		/* init all nodes and their dependencies to 0 */
		if (map_get(indegrees, e.key) != NULL) {
			continue;
		}
		map_set(indegrees, dynamic_str(e.key), dynamic_int(0));
		for (int j = 0; j < string_arr_n(deps); j++) {
			char *dep_key = string_arr_s(deps)[j]; 
			if (map_get(indegrees, dep_key) != NULL) {
				continue;
			}		
			map_set(indegrees, dynamic_str(dep_key), dynamic_int(0));
		}
	}

	for (int i = 0; i < indegrees->n; i++) {
		struct entry e = indegrees->entry[i];
		struct string_arr *n_arr = map_get(g, e.key);
		if (!n_arr) {
			continue;
		}
		for (int j = 0; j < string_arr_n(n_arr); j++) {
			int *count = (int *) map_get(indegrees, e.key);
			*count = *count + 1; /* XXX */
		}
	}
	return indegrees;
}

static int *
dynamic_int(int i)
{
	int *val = malloc(sizeof(int));
	*val = i;
	return val;
}

static struct string_arr *
build_indegree_zero(struct map *indegrees)
{
	struct string_arr *indegree_zero = string_arr_create();
	for (int i = 0; i < indegrees->n; i++) {
		struct entry e = indegrees->entry[i];
		int *val = (int *) map_get(indegrees, e.key);
		if (*val == 0) {
			string_arr_append(
				indegree_zero,
				dynamic_str(e.key)
			);
		}
	}
	return indegree_zero;
}
