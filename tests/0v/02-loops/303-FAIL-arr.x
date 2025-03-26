void
unit()
{
	int i;
	int arr[1];

	arr[0] = 0;

	for (i = 0; i < 1; i++) ~ [ i = [0?2]; ] /* asserts invariance of arr */
		arr[i] = 1;
}
