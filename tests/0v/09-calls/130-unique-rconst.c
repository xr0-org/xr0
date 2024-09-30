#ifdef XR0

int
f(int x) ~ [ return [0?2]; ];

#endif

int
f(int x)
{
	return 0;
}

#ifdef XR0

int
g(int x, int y) ~ [
	setup: {
		x = [0?2];
		y = [0?2];
	}
];

#endif

int
g(int x, int y)
{
	int a; int b;

	if (x != y) {
		return 0;
	}

	a = f(x); /* "f:{($0, [?])}:0" */
	b = f(y); /* "f:{($1, [?])}:0" */

	#ifdef XR0
	~ [ a == b; ]
	#endif
}
