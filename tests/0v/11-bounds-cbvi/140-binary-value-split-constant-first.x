foo(int index) ~ [ setup: index = [0?2]; ]
{
	int arr[2];
	arr[0] = 0;
	arr[index] = 1;
	if (arr[0]) {
		~ [ index == 0; ]
	} else {
		~ [ index == 1; ]
	}
}
