void
modify(int *q) ~ [
	pre: {
		q = .clump(sizeof(int));
	}
	*q = 2;
] {
	*q = 2;
}

int *
main()
{
	int p;
	p = 1;
	~ [ p == 1; ];
	modify(&p);
	~ [ p == 2; ];
}
