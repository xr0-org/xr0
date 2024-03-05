void
assign(int *q) ~ [
	pre: *q;	/* no side effect */
] {
	int p;
	p = *q;
}

int *
main()
{
	int p;
	p = 1;
	assign(&p);
	~ [ p = 1; ];
}
