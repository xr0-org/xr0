foo(int index) ~ [ setup: index = [0?2]; ]
{
	int x;
	int arr[2];
	arr[index] = 2;
        arr[0] = 1;
        ~ [ arr[0] == 1; ]
        x = arr[0];
	if (index) {
		~ [ arr[1] == 2; ]
		x = arr[1];
	}
}
