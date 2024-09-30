void
access(int *arr) ~ [
	setup: arr = .clump(5) - 5;
	arr[5] = 1;
	arr[7] = 1;
]{
	arr[5] = 1;
	arr[7] = 1;
}
