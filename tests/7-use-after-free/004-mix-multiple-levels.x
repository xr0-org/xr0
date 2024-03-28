int
snapshot_and_change(int *arg) ~ [
	setup: {
		arg = .clump(sizeof(int));
		*arg = $;
	}
	return *arg;
	*arg = 3;
] {
	int j;
	j = *arg;
	*arg = 3;
	return j;
}

void
modify(int *p, int *q) ~ [
	int i;
	setup: {
		p = .clump(sizeof(int));
		*p = $;
		q = .clump(sizeof(int));
	};
	*q = 2;
	*p = 3;
	i = *p;
] {
	int i;
	i = 0;
	i = snapshot_and_change(p);
	~ [ *p == 3; ];

	*q = 2;
}

int *
main()
{
	int p;
	int q;
	p = 9;
	~ [ p == 9; ];
	modify(&p, &q);
	~ [ p == 3; ];
	~ [ q == 2; ];
}
