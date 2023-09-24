#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "heap.h"
#include "block.h"
#include "location.h"
#include "object.h"
#include "util.h"
#include "value.h"

#define LEN(a) (sizeof(a) / sizeof((a)[0]))

char *
test_value();

char *
test_object();

char *
test_block();

char *
test_location();

char *
test_heap();

/* all tests */

int
main()
{
	typedef char * (*test)();

	struct { char *name; test test; } tests[] = {
		{ "test_value",		test_value, },
		{ "test_object",	test_object, },
		{ "test_block",		test_block, },
		{ "test_location",	test_location, },
		{ "test_heap",		test_heap, },
	};

	for (int i = 0, len = LEN(tests); i < len; i++) {
		char *fail = NULL;
		if ((fail = tests[i].test())) {
			fprintf(stderr, "%s failed: %s\n", tests[i].name, fail);
			return EXIT_FAILURE;
		}
	}

	return 0;
}

/* value tests */

bool
test_value_ptr();

bool
test_value_virt();

char *
test_value()
{
	typedef bool (*test)();

	struct { char *name; test test; } tests[] = {
		{ "ptr",	test_value_ptr, },
		{ "virt",	test_value_virt, },
	};

	for (int i = 0, len = LEN(tests); i < len; i++) {
		if (!tests[i].test()) {
			return tests[i].name;
		}
	}

	return NULL;
}

bool
test_value_virt()
{
	struct location *loc = location_create(
		LOCATION_AUTOMATIC, 7, ast_expr_create_constant(3)
	);
	struct value *val = value_virt_create(loc);

	char *s = value_str(val);
	if (strcmp(s, "virt:stack:7+3") != 0) {
		return false;
	}
	free(s);

	if (value_as_virt(val) != loc) {
		return false;
	}

	value_destroy(val);

	return true;
}

bool
test_value_ptr()
{
	struct location *loc = location_create(
		LOCATION_AUTOMATIC, 3, ast_expr_create_constant(4)
	);
	struct value *val = value_ptr_create(loc);

	char *s = value_str(val);
	if (strcmp(s, "ptr:stack:3+4") != 0) {
		return false;
	}
	free(s);

	if (value_as_ptr(val) != loc) {
		return false;
	}

	value_destroy(val);

	return true;
}


/* object tests */

bool
test_object_create();

bool
test_object_contains();

bool
test_object_isempty();

bool
test_object_isvirtual();

bool
test_object_contig_precedes();

bool
test_object_upto();

bool
test_object_from();

char *
test_object()
{
	typedef bool (*test)();

	struct { char *name; test test; } tests[] = {
		{ "test_object_create",			test_object_create, },
		{ "test_object_contains",		test_object_contains, },
		{ "test_object_isempty",		test_object_isempty, },
		//{ "test_object_isvirtual",		test_object_isvirtual, },
		{ "test_object_contig_precedes",	test_object_contig_precedes, },
		{ "test_object_upto",			test_object_upto, },
		{ "test_object_from",			test_object_from, },
	};

	for (int i = 0, len = LEN(tests); i < len; i++) {
		if (!tests[i].test()) {
			return tests[i].name;
		}
	}

	return NULL;
}


static struct object *
setup_object_single()
{
	return object_create(ast_expr_create_constant(0), ast_expr_create_constant(1));
}

static struct object *
setup_object_empty()
{
	return object_create(ast_expr_create_constant(0), ast_expr_create_constant(0));
}

static struct object *
setup_object_with_value()
{
	struct object *obj = object_create(ast_expr_create_constant(0), ast_expr_create_constant(1));

	struct location *loc = location_create(
		LOCATION_DYNAMIC, 3, ast_expr_create_constant(0)
	);
	struct value *val = value_ptr_create(loc);
	object_assign(obj, val);

	return obj;
}

static struct object *
setup_object_range()
{
	return object_create(ast_expr_create_constant(0), ast_expr_create_constant(6));
}

static struct object *
setup_object_range_proceding()
{
	return object_create(ast_expr_create_constant(6), ast_expr_create_constant(10));
}

bool
test_object_create()
{
	struct object *obj = setup_object_single();

	char *s = object_str(obj);
	printf("%s\n", s);
	free(s);

	object_destroy(obj);
	return true;
}

bool
test_object_contains()
{
	struct object *obj = setup_object_range();	
	struct ast_expr *valid_offset = ast_expr_create_constant(0),
			*invalid_offset = ast_expr_create_constant(7);

	bool pos_test = object_contains(obj, valid_offset),
	     neg_test = object_contains(obj, invalid_offset);

	ast_expr_destroy(invalid_offset); ast_expr_destroy(valid_offset);
	object_destroy(obj);

	return !neg_test && pos_test;
}

bool
test_object_isempty()
{
	struct object *obj_nonempty = setup_object_single(),
		      *obj_empty = setup_object_empty();	
	
	bool neg_test = object_isempty(obj_nonempty),
	     pos_test = object_isempty(obj_empty);

	object_destroy(obj_empty); object_destroy(obj_nonempty);

	return !neg_test && pos_test;
}

bool
test_object_isvirtual()
{
	struct object *obj_null = setup_object_single();
	struct object *obj_real = setup_object_with_value();
	struct object *obj_virt = setup_object_range();

	bool neg_test1 = object_isvirtual(obj_null),
	     neg_test2 = object_isvirtual(obj_real),
	     pos_test = object_isvirtual(obj_virt);

	object_destroy(obj_virt); object_destroy(obj_real); object_destroy(obj_null);

	return !neg_test1 && !neg_test2 && pos_test;
}

bool
test_object_contig_precedes()
{
	struct object *obj_notpreceding = setup_object_single(),
		      *obj_preceding = setup_object_range(),
		      *obj_proceding = setup_object_range_proceding();
	
	bool neg_test = object_contig_precedes(obj_notpreceding, obj_proceding),
	     pos_test = object_contig_precedes(obj_preceding, obj_proceding);

	object_destroy(obj_notpreceding); object_destroy(obj_proceding); object_destroy(obj_preceding);

	return !neg_test && pos_test;
}

bool
test_object_upto()
{
	struct object *obj = setup_object_range();
	struct heap *heap = heap_create();
	
	struct ast_expr *excl_upper = ast_expr_create_constant(4);
	struct object *res = object_upto(obj, excl_upper, heap);
	
	struct ast_expr *expected_lower = ast_expr_create_constant(0),
			*expected_upper = ast_expr_create_constant(4);

	printf("lower: %s, upper: %s\n",
		ast_expr_str(object_lower(res)),
		ast_expr_str(object_upper(res))
	);
	bool result = ast_expr_equal(object_lower(res), expected_lower)
		&& ast_expr_equal(object_upper(res), expected_upper);	

	ast_expr_destroy(expected_upper); ast_expr_destroy(expected_lower);
	ast_expr_destroy(excl_upper);
	heap_destroy(heap);

	return result;
}

bool
test_object_from()
{
	struct object *obj = setup_object_range();
	struct heap *heap = heap_create();
	
	struct ast_expr *incl_lower = ast_expr_create_constant(4);
	struct object *res = object_from(obj, incl_lower, heap);
	
	struct ast_expr *expected_lower = ast_expr_create_constant(4),
			*expected_upper = ast_expr_create_constant(6);

	bool result = ast_expr_equal(object_lower(res), expected_lower)
		&& ast_expr_equal(object_upper(res), expected_upper);	

	ast_expr_destroy(expected_upper); ast_expr_destroy(expected_lower);
	ast_expr_destroy(incl_lower);
	heap_destroy(heap);

	return result;
}

/* test block */

bool
test_block_create();

bool
test_block_observe();

bool
test_block_range_aredeallocands();

bool
test_block_range_dealloc();

char *
test_block()
{
	typedef bool (*test)();

	struct { char *name; test test; } tests[] = {
		{ "block_create",		test_block_create, },
		{ "block_observe",		test_block_observe, },
		{ "block_range_aredeallocands",	test_block_range_aredeallocands, },
		{ "block_range_dealloc",	test_block_range_dealloc, },
	};

	for (int i = 0, len = LEN(tests); i < len; i++) {
		if (!tests[i].test()) {
			return tests[i].name;
		}
	}

	return NULL;
}

bool
test_block_create()
{
	block *obj = block_create();

	char *s = block_str(obj);
	printf("%s\n", s);
	free(s);

	block_destroy(obj);
	return true;
}

bool
test_block_observe()
{
	struct heap *heap = heap_create();

	block *b = block_create();
	struct ast_expr *lower = ast_expr_create_constant(0),
			*upper = ast_expr_create_constant(9),
			*offset = ast_expr_create_constant(4);

	struct error *err = block_range_alloc(b, lower, upper, heap);	
	if (err) {
		return false;
	}

	/* assert 1 object in block */
	int nobjects = object_arr_nobjects(b);	
	if (nobjects != 1) {
		return false;
	}

	/* observe */
	struct object *obs = block_observe(b, offset, heap);	

	/* assert 3 objects in block after observation */
	nobjects = object_arr_nobjects(b);
	if (nobjects != 3) {
		return false;	
	}

	/* check bounds */
	struct object **objects = object_arr_objects(b);
	struct object *before = objects[0];

	struct ast_expr *expected_before_lower = ast_expr_create_constant(0),
			*expected_before_upper = ast_expr_create_constant(4);
	if (!ast_expr_equal(object_lower(before), expected_before_lower)) {
		return false;
	}
	if (!ast_expr_equal(object_upper(before), expected_before_upper)) {
		return false;
	}
	
	struct ast_expr *expected_obs_upper = ast_expr_create_binary(
		ast_expr_create_constant(4),
		BINARY_OP_ADDITION,
		ast_expr_create_constant(1)
	);
	if (!ast_expr_equal(object_lower(obs), expected_before_upper)) {
		return false;
	}
	if (!ast_expr_equal(object_upper(obs), expected_obs_upper)) {
		return false;
	}
	
	struct object *after = objects[2];
	if (!ast_expr_equal(object_lower(after), expected_obs_upper)) {
		return false;
	}
	if (!ast_expr_equal(object_upper(after), ast_expr_create_constant(9))) {
		return false;
	}

	/*
	ast_expr_destroy(expected_obs_upper);
	ast_expr_destroy(expected_before_upper);
	ast_expr_destroy(expected_before_lower);

	ast_expr_destroy(offset);
	ast_expr_destroy(upper);
	ast_expr_destroy(lower);

	block_destroy(b);

	heap_destroy(heap);
	*/

	return true;
}

bool
test_block_range_aredeallocands()
{
	struct heap *heap = heap_create();

	block *b = block_create();
	struct ast_expr *lower_alloc = ast_expr_create_constant(0),
			*upper_alloc = ast_expr_create_constant(9);

	struct error *err = block_range_alloc(b, lower_alloc, upper_alloc, heap);	
	if (err) {
		return false;
	}

	bool res = block_range_aredeallocands(b, lower_alloc, upper_alloc, heap);

	ast_expr_destroy(upper_alloc);
	ast_expr_destroy(lower_alloc);
	block_destroy(b);
	heap_destroy(heap);

	return res;
}


bool
test_block_range_dealloc()
{
	struct heap *heap = heap_create();

	block *b = block_create();
	struct ast_expr *lower_alloc = ast_expr_create_constant(0),
			*upper_alloc = ast_expr_create_constant(9),
			*lower_dealloc = ast_expr_create_constant(4),
			*upper_dealloc = ast_expr_create_constant(7);

	struct error *err = block_range_alloc(b, lower_alloc, upper_alloc, heap);	
	if (err) {
		return false;
	}
	
	err = block_range_dealloc(b, lower_dealloc, upper_dealloc, heap);
	if (err) {
		return false;
	}

	return block_range_aredeallocands(b, lower_alloc, lower_dealloc, heap)
		&& !block_range_aredeallocands(b, lower_dealloc, upper_dealloc, heap)
		&& block_range_aredeallocands(b, upper_dealloc, upper_alloc, heap);
}

/* test location */

bool
test_location_create();

char *
test_location()
{
	typedef bool (*test)();

	struct { char *name; test test; } tests[] = {
		{ "location_create",		test_location_create, },
	};

	for (int i = 0, len = LEN(tests); i < len; i++) {
		if (!tests[i].test()) {
			return tests[i].name;
		}
	}

	return NULL;
}

bool
test_location_create()
{
	struct ast_expr *offset = ast_expr_create_constant(0);
	struct location *loc = location_create(LOCATION_DYNAMIC, 0, offset);

	char *s = location_str(loc);
	printf("%s\n", s);
	free(s);

	ast_expr_destroy(offset);
	location_destroy(loc);
	return true;
}


/* test heap */

bool
test_heap_create();

bool
test_heap_newblock();

bool
test_heap_getblock();

bool
test_heap_deallocblock();

bool
test_heap_free_range();

char *
test_heap()
{
	typedef bool (*test)();

	struct { char *name; test test; } tests[] = {
		{ "heap_create",	test_heap_create, },
		{ "heap_newblock",	test_heap_newblock, },
		{ "heap_getblock",	test_heap_getblock, },
		{ "heap_deallocblock",	test_heap_deallocblock, },
	};

	for (int i = 0, len = LEN(tests); i < len; i++) {
		if (!tests[i].test()) {
			return tests[i].name;
		}
	}

	return NULL;
}

bool
test_heap_create()
{
	struct heap *heap = heap_create();
	
	char *s = heap_str(heap);
	printf("%s\n", heap_str(heap));
	free(s);

	heap_destroy(heap);
	return true;
}

bool
test_heap_newblock()
{
	struct heap *heap = heap_create();
	struct location *loc0 = heap_newblock(heap, 1),
			*loc1 = heap_newblock(heap, 1);
	
	/*
	if (location_block(loc0) != 0) {
		return false;
	}
	if (location_block(loc1) != 1) {
		return false;
	}
	*/

	location_destroy(loc0);
	location_destroy(loc1);
	heap_destroy(heap);
	return true;
}

bool
test_heap_getblock()
{
	struct heap *heap = heap_create();
	struct location *loc = heap_newblock(heap);
	
	block *b = location_getblock(loc, NULL, heap);
	
	bool pos_test = (bool) b;

	heap_destroy(heap);

	return pos_test;
}

bool
test_heap_deallocblock()
{
	struct heap *heap = heap_create();
	struct location *loc = heap_newblock(heap);
	
	struct error *err = location_dealloc(loc, heap),
		     *double_free = location_dealloc(loc, heap);

	heap_destroy(heap);

	return !err && double_free;
}
