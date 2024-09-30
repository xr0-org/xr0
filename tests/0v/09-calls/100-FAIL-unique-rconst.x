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
g()
{
	int a; int b;

	a = f(0); /* "f{0}:0" */
	b = f(1); /* "f{1}:0" */

	#ifdef XR0
	~ [ a != b; ]
	#endif
}
