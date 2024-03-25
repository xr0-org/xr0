void
modify2(int *q, int x) ~ [
	pre: if (x) { q = .clump(1); } /* ERROR: setup must be decidable */
	if (x) {
		*q = 2;
	}
] {
	if (x) {
		*q = 2;
	}
}
