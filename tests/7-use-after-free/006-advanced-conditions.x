void
modifyY(int *q, int x, int y) ~ [
	if (x) {
		pre: .clump q;
		*q = y;
	}
] {
	if (x) {
		*q = y;
	}
}

void
modify1(int *q, int c) ~ [
	if (c) {
		pre: .clump q;
		*q = 1;
	}
] {
	modifyY(q, c, 1);
}

void
modify2(int *q, int c) ~ [
	modifyY(q, c, 2);
] {
	if (c) {
		*q = 2;
	}
}

void
modify3(int *q, int c) ~ [
	modifyY(q, c, 3);
] {
	modifyY(q, c, 3);
}

int *
main()
{
	int p;
	p = 0;
	~ [ p == 0; ];
	modify1(&p, 0);
	~ [ p == 0; ];
	modify1(&p, 1);
	~ [ p == 1; ];
	modify2(&p, 0);
	~ [ p == 1; ];
	modify2(&p, 1);
	~ [ p == 2; ];
	modify3(&p, 0);
	~ [ p == 2; ];
	modify3(&p, 1);
	~ [ p == 3; ];
}
