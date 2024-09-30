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
	k = 0;
	getval(&k-1);
}
