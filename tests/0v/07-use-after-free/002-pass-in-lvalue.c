#ifdef XR0

void
modify(int *q) ~ [
	setup: q = .clump(sizeof(int));
	*q = 2;
];

#endif

void
modify(int *q)
{
	*q = 2;
}

int
main()
{
	int p;
	p = 1;

	#ifdef XR0
	~ [ p == 1; ];
	#endif

	modify(&p);

	#ifdef XR0
	~ [ p == 2; ];
	#endif

	return 0;
}
