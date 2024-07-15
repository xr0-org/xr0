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
	/* TODO: fix by changing to .x after ~ [] works again */
	~ [ tuple_create().x == tuple_create().y; ]
}

struct tuple
tuple_create()
{
	struct tuple t;

	t.x = 0;
	t.y = 0;
	return t;
}
