void
modify0(int *q, int x) ~ [
	setup: q = .clump(1);
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
		setup: q = .clump(1);
		*q = 2;
	}
] {
	if (x) {
		*q = 2;
	}
}

int
main()
{
	int p;
	p = 0;
	~ [ p == 0; ]
	modify0(&p, 0);
	~ [ p == 0; ]
	modify0(&p, 1);
	~ [ p == 1; ]
	modify1(&p, 0);
	~ [ p == 1; ]
	modify1(&p, 1);
	~ [ p == 2; ]
	return 0;
}
