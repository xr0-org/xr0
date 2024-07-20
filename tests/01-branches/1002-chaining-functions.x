struct tuple { int x; int y; };

struct tuple
tuple_create() ~ [
	struct tuple t;
	t.x = $;
	t.y = $;
	return t;
];

test()
{
	struct tuple a;
	struct tuple b;

	a = tuple_create();
	b = tuple_create();
	~ [ a.x == b.x; ]
}

struct tuple
tuple_create()
{
	struct tuple t;

	t.x = 0;
	t.y = 0;
	return t;
}
