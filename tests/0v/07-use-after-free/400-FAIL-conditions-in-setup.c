#ifdef XR0

void
modify2(int *q, int x) ~ [
	setup: if (x) { q = .clump(1); } /* ERROR: setup must be decidable */
	if (x) {
		*q = 2;
	}
];

#endif

void
modify2(int *q, int x)
{
	if (x) {
		*q = 2;
	}
}
