#ifdef XR0

void
foo(int index) ~ [ setup: index = 0; ];

#endif

void
foo(int index)
{
	int k;
	(&k)[index] = index;
}
