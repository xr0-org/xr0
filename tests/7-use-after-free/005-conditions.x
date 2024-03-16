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
		*q = 2;
	}
] {
	if (x) {
		*q = 2;
	}
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
}
