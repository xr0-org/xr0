void
modify(int *q) ~ [
	*q = $;
] {
	*q = 1;
}

int *
main()
{
	int p;
	p = 1;
	~ [ p == 1; ];
	modify(&p);
	~ [ p == $; ];
}
