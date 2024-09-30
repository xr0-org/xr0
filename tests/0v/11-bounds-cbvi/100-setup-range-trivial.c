#ifdef XR0

void
foo(int index) ~ [ setup: index = [1?2]; ];

#endif

void
foo(int index)
{
	int k;
	(&k)[index-1] = index;
}
