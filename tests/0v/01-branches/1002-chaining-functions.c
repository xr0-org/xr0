struct tuple { int x; int y; };

#ifdef XR0

struct tuple
tuple_create() ~ [
	struct tuple t;
	t.x = [?];
	t.y = [?];
	return t;
];

#endif

test()
{
	struct tuple a;
	struct tuple b;

	a = tuple_create();
	b = tuple_create();

	#ifdef XR0
	~ [ a.x == b.x; ];
	#endif
}

struct tuple
tuple_create()
{
	struct tuple t;

	t.x = 0;
	t.y = 0;
	return t;
}
