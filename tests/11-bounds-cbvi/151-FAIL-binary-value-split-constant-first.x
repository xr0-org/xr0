foo(int index) ~ [ setup: index = [0?2]; ]
{
	int k, arr[2];
        arr[1] = 1;
        k = arr[index]; /* arr[0] not defined */
}
