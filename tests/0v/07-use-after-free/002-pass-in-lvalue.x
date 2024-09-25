void
modify(int *q) ~ [
	setup: q = .clump(sizeof(int));
	*q = 2;
] {
	*q = 2;
}

int
main()
{
	int p;
	p = 1;
	~ [ p == 1; ];
	modify(&p);
	~ [ p == 2; ];
	return 0;
}
