void
unit()
{
	int i; int j;
	int arr[2];

	for (i = 0; i < 2; i++) ~ [
		i = [0?2];
		for (j = 0; j < i; j++) {
			if (j == 0) {
				arr[j] = 1;
			}
			if (j == 1) {
				arr[j] = 2;
			}
		}
	] {
		if (i == 0) {
			arr[i] = 1;
		}
		if (i == 1) {
			arr[i] = 2;
			break;
		}
	}

	~ [ 0 <= i; i <= 2; ]
	~ [ 1 <= arr[0]; arr[0] <= 1; ]
	~ [ 2 <= arr[1]; arr[1] <= 2; ]
}
