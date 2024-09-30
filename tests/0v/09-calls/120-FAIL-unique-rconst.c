#ifdef XR0

int
f(int x) ~ [ return [0?2]; ];

#endif

int
f(int x)
{
	return 0;
}

int
g(int x, int y)
{
	int a; int b;

	a = f(x); /* "f:{($0, [?])}:0" */
	b = f(y); /* "f:{($1, [?])}:0" */

	#ifdef XR0
	~ [ a == b; ]
	#endif
}
