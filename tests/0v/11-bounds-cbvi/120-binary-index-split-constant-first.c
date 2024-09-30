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
	if (index) {
		#ifdef XR0
		~ [ arr[0] == 1; arr[1] == 2; ]
		#endif

		arr[1];
	} else {
		#ifdef XR0
		~ [ arr[0] == 2; ]
		#endif

		arr[0];
	}
}
