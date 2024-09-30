#ifdef XR0

void
foo(int index) ~ [ setup: index = [0?2]; ];

#endif

void
foo(int index)
{
	int arr[2];
	arr[0] = 0;
	arr[index] = 1;
	if (arr[0]) {
		#ifdef XR0
		~ [ index == 0; ]
		#endif
	} else {
		#ifdef XR0
		~ [ index == 1; ]
		#endif
	}
}
