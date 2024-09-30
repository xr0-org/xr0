#ifdef XR0

void
modify0(int *q, int x) ~ [
	setup: q = .clump(1);
	if (x) {
		*q = 1;
	}
];

#endif

void
modify0(int *q, int x)
{
	if (x) {
		*q = 1;
	}
}

#ifdef XR0

void
modify1(int *q, int x) ~ [
	if (x) {
		setup: q = .clump(1);
		*q = 2;
	}
];

#endif

void
modify1(int *q, int x)
{
	if (x) {
		*q = 2;
	}
}

int
main()
{
	int p;
	p = 0;

	#ifdef XR0
	~ [ p == 0; ]
	#endif

	modify0(&p, 0);

	#ifdef XR0
	~ [ p == 0; ]
	#endif

	modify0(&p, 1);

	#ifdef XR0
	~ [ p == 1; ]
	#endif

	modify1(&p, 0);

	#ifdef XR0
	~ [ p == 1; ]
	#endif

	modify1(&p, 1);

	#ifdef XR0
	~ [ p == 2; ]
	#endif

	return 0;
}
