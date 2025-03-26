void
unit()
{
	int i;
	int arr[2];

	for (i = 0; i < 2; i++) ~ [
		i = 0;
	] {
		if (i == 0) {
			arr[i] = 1;
			break;
		};
	}

	~ [ 0 <= i; i <= 0; ]
	~ [ 1 <= arr[0]; arr[0] <= 1; ]
}
