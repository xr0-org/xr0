void
modify2(int *q, int x) ~ [
	pre: if (x) { .clump q; } /* ERROR: setup must be decidable */
	if (x) {
		*q = 2;
	}
] {
	if (x) {
		*q = 2;
	}
}
