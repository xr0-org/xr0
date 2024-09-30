#ifdef XR0

int
snapshot_and_change(int *arg) ~ [
	int j;
	setup: {
		arg = .clump(sizeof(int));
		*arg = [?];
	}
	j = *arg;
	*arg = 3;
	return j;
];

#endif

int
snapshot_and_change(int *arg)
{
	int j;
	j = *arg;
	*arg = 3;
	return j;
}

#ifdef XR0

void
modify(int *p, int *q) ~ [
	int i;
	setup: {
		p = .clump(sizeof(int));
		*p = [?];
		q = .clump(sizeof(int));
	};
	*q = 2;
	*p = 3;
	i = *p;
];

#endif

void
modify(int *p, int *q)
{
	int i;
	i = 0;
	i = snapshot_and_change(p);

	#ifdef XR0
	~ [ *p == 3; ];
	#endif

	*q = 2;
}

int
main()
{
	int p;
	int q;
	p = 9;

	#ifdef XR0
	~ [ p == 9; ];
	#endif

	modify(&p, &q);

	#ifdef XR0
	~ [ p == 3; ];
	~ [ q == 2; ];
	#endif

	return 0;
}
