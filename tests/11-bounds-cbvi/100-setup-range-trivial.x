void
foo(int index) ~ [ setup: index = [1?2]; ]
{
	int k;
	(&k)[index-1] = index;
}
