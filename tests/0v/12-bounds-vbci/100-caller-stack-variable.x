void
access(int *arr) ~ [
	setup: arr = .clump(1) - 1;
	arr[1] = 1;
]{
	arr[1] = 1;
}

void
f()
{
	int k;
	access(&k-1);
}
