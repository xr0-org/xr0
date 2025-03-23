void
modify0(int *q, int x) ~ [
	setup: q = .clump(1);
	if (x) {
		*q = 1;
	}
] {
	if (x) {
		*q = 1;
	}
}
