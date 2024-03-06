int
snapshot_and_change(int *arg) ~ [
	pre: {
		.clump arg;
		*arg = $;
	}
	result = *arg;
	*arg2 = 2;
] {
	int j;
	j = *arg;
	*arg = 2;
	return j;
}

void
modify(int *p, int *q) ~ [
	pre: {
		.clump p;
		*p = $;
		.clump q;
	};
	*q = $;
] {
	int i;
	i = 0;
	i = snapshot_and_change(p);
	~ [ i == 1; ];
	~ [ *p == 2; ];

	*q = i;
}

int *
main()
{
	int p;
	int q;
	p = 9;
	~ [ p == 9; ];
	modify(&p, &q);
	~ [ p == 2; ];
	~ [ q == 1; ];
}
