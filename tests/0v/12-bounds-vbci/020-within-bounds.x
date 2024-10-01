void
access(int *arr) ~ [
	setup: arr = .clump(2);
	arr[1] = 1;
]{
	arr[1] = 1;
}
