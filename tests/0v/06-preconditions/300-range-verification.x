#ifdef XR0

int
foo(int x) ~ [ setup: x = [0?2]; ];

#endif

int
foo(int x)
{
	return 3;
}

int
bar(int x)
{
	foo(1);
	return 0;
}
