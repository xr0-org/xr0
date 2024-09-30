#ifdef XR0

void
foo(int i) ~ [ setup: i = [0?5]; ];

#endif

void
foo(int i)
{
	int arr[2];
	if (i <= 1) {
		arr[i] = 0;
	}
}

#ifdef XR0

void
bar(int i) ~ [ setup: i = [0?5]; ];

#endif

void
bar(int i)
{
	int arr[2];
	if (!(i > 1)) {
		arr[i] = 0;
	}
}
