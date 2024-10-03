int
f(int *arr) ~ [
	setup: {
		arr = .clump(2);
		arr[0] = [?];
		arr[1] = [?];
	}
	return arr[0];
]{
	int k;
	k = arr[1];
	return arr[0];
}

void
g()
{
	int arr[2];

	arr[0] = 0;
	f(arr);
}
