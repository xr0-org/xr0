void
modify(int *q) ~ [
	pre: .clump q;
	*q = 1;
] {
	*q = 1;
}

int *
main()
{
	int p;
	p = 1;
	~ [ p == 1; ];
	modify(&p);
	~ [ p == 1; ];
}
