void
modify0(int *q, int x) ~ [
	pre: .clump q;
	if (x) {
		*q = 1;
	}
] {
	if (x) {
		*q = 1;
	}
}

void
modify1(int *q, int x) ~ [
	if (x) {
		pre: .clump q;
		*q = 3;
	}
] {
	if (x) {
		*q = 3;
	}
	/* XXX: add failure case for if there's more if on x that means setups need
	 * to be deduped so they only run once
	 */
}

int *
main()
{
	int p;
	p = 0;
	~ [ p == 0; ];
	modify0(&p, 0);
	~ [ p == 0; ];
	modify0(&p, 1);
	~ [ p == 1; ];
	modify1(&p, 0);
	~ [ p == 1; ];
	modify1(&p, 1);
	~ [ p == 2; ];
	modify2(&p, 0);
	~ [ p == 2; ];
	modify2(&p, 1);
	~ [ p == 3; ];
}
