#ifdef XR0

void
foo(int index) ~ [ setup: index = [0?2]; ];

#endif

void
foo(int index)
{
	int k, arr[2];
        arr[1] = 1;
        k = arr[index]; /* arr[0] not defined */
}
