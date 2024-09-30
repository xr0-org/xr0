#ifdef XR0

void
foo(int index) ~ [ setup: index = [0?2]; ];

#endif

void
foo(int index)
{
	int arr[2];
	arr[0] = 1;
	arr[index] = 2;
}
