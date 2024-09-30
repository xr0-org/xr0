#ifdef XR0

void
foo(int index) ~ [ setup: index = [0?2]; ];

#endif

void
foo(int index)
{
	int x;
	int arr[2];
	arr[index] = 2;
        arr[0] = 1;

	#ifdef XR0
        ~ [ arr[0] == 1; ]
	#endif

        x = arr[0];
	if (index) {
		#ifdef XR0
		~ [ arr[1] == 2; ]
		#endif

		x = arr[1];
	}
}
