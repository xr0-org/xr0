int
assign(int *q) ~ [
	pre: {
		q = .clump(1);
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
