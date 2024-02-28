int
assign(int *q) ~ [
	pre: {
		.clump q;	/* no side effect */
		*q = $;
	}
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
	~ [ p == 1; ];
}
