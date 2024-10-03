int
getval(int *arr) ~ [
	setup: {
		arr = .clump(1) - 1;
		arr[1] = [?];
	}
	return arr[1];
]{
	return arr[1];
}

void
f()
{
	int k;
	getval(&k-1);
}
