#ifdef XR0

void
assign(int *q) ~ [
	setup: {
		q = .clump(sizeof(int));
		*q = [?];
	}
];

#endif

void
assign(int *q)
{
	int p;
	p = *q;
}

int
main()
{
	int p;
	p = 1;
	assign(&p);

	#ifdef XR0
	~ [ p == 1; ];
	#endif

	return 0;
}
