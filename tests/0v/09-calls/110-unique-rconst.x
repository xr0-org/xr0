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

	a = f(0);
	b = f(0);

	#ifdef XR0
	~ [ a == b; ]
	#endif

	return 0;
}
