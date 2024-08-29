void
foo(int index) ~ [ setup: index = 0; ]
{
	int k;
	(&k)[index] = index;
}
