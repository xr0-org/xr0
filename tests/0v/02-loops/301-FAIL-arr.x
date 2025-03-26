void
unit()
{
	int i;
	int arr[2];

	arr[0] = arr[1] = 0;
	for (i = 0; i < 2; i++) ~ [ i = [0?3]; ] /* asserts arr[1] == 0 */
		arr[i] = i;
}
