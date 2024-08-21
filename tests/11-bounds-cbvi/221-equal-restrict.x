foo(int i) ~ [ setup: i = [0?5]; ]
{
	int arr[2];
	if (i == 1) {
		arr[i] = 0;
	}
}
