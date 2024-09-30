#ifdef XR0

int
foo(int x) ~ [ setup: x = [-1?2]; ];

#endif

int
foo(int x)
{
	return 3;
}

int
bar(int x)
{
	foo(-2); /* FAIL */
}
