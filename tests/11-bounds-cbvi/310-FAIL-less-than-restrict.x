void
foo(int i) ~ [ setup: i = [0?5]; ]
{
	int arr[2];
	if (i < 3) {
		arr[i] = 0;
	}
}