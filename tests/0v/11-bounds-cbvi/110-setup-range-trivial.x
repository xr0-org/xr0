void
foo(int index) ~ [ setup: index = [0?2]; ]
{
	int arr[2];
	arr[0] = 1;
	arr[index] = 2;
}