void
foo(int index) ~ [ setup: index = [0?1]; ]
{
	int k;
	(&k)[index] = index;
}
