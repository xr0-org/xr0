#ifdef XR0

void
foo(int i) ~ [ setup: i = [0?5]; ];

#endif

void
foo(int i)
{
	int arr[2];
        arr[i] = 0;
}
