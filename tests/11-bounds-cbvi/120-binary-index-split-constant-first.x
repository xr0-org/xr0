foo(int index) ~ [ setup: index = [0?2]; ]
{
	int arr[2];
	arr[0] = 1;
	arr[index] = 2;
	if (index) {
		~ [ arr[0] == 1; arr[1] == 2; ]
		arr[1];
	} else {
		~ [ arr[0] == 2; ]
		arr[0];
	}
}