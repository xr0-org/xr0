void
assign(int *q) ~ [
	setup: {
		q = .clump(sizeof(int));
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
